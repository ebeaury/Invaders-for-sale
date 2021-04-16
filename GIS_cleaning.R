rm(list=ls())

#load packages
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Eve/Box/Invaders for Sale/final database results")
setwd("/Users/ebeaury/Box/Invaders for Sale/final database results")


#
#
#

pio <- read.csv("vendors.pio.csv")
gog <- read.csv("vendors.google.csv")
sp <- read.csv("species.list.google.csv")

setwd("C:/Users/ebeaury/Box/Invaders for Sale/final database results/spatial data")
master <- read.csv("master_vendor_list_GIS_3.4.2020.csv")
head(master)
str(master)

# export new GIS datasheet

# Drop columns needing updates from master
master_new <- master[,1:6]
head(master_new)

head(gog)
gog.vens <- gog %>% select(Vendor) 

head(pio)
pio.vens <- pio %>% select(Vendor)

both.vens <- rbind(gog.vens, pio.vens)
both.vens <- unique(both.vens) # Should be number of vendors currently in master
str(master_new) # Need to drop some
length(levels(master_new$Vendor))

master_new$keep <- ifelse(master_new$Vendor %in% both.vens$Vendor, "Y", "N")
table(master_new$keep)

# Drop anything with keep = N
master_new2 <- master_new %>% filter(keep=="Y") %>% select(-keep) %>% droplevels()
head(master_new2)
str(master_new2)

# Add column to master new to indicate these are all nurseries
# master_new$Vendor.Type = "NURSERY"
# master_new <- master_new[,c(1,6,2:5)]

# Now, need to add non-nurseries with location information to this list
# head(gog)
# head(master_new)
#nons <- gog %>% filter(Vendor.Type=="NON-NURSERY", !Latitude=="UNKNOWN") %>% 
  #select(Vendor, Vendor.Type, Address, Address.State, Latitude, Longitude) %>%
  #rename(Address_St=Address.State)
#nons <- droplevels(unique(nons))
#df <- data.frame(table(nons$Vendor)) # Double check no duplicates

# Now add non-nurseries to master new
#master_new2 <- rbind(master_new, nons)

# Count of species for sale by each nursery in both databases
  # Exotics for sale
  # Natives + Other + Both for sale
  # Noxious weeds for sale
  # State listed species for sale

# Make the dataframe
both <- rbind(gog %>% select(USDA.Code, Vendor, Address.State), 
              pio %>% select(USDA.Code, Vendor, Address.State)) %>% distinct()
both <- droplevels(both)

# get us nativity
exos <- sp %>% select(USDA.Code, US.Nativity) %>% distinct()
both <- left_join(both, exos, by="USDA.Code")

# get noxious weed status
nws <- sp %>% select(USDA.Code, USDA.) %>% distinct()
both <- left_join(both, nws, by="USDA.Code")

# state listed count
sl <- sp %>% select(USDA.Code, State.Listed) %>% distinct()
both <- left_join(both, sl, by="USDA.Code")

# number sold in state where invasive
sis <- rbind(gog %>% select(USDA.Code, Vendor, Address.State, Sold.In.State),
             pio %>% select(USDA.Code, Vendor, Address.State, Sold.In.State)) %>% distinct()
both <- left_join(both, sis, by=c("USDA.Code", "Vendor", "Address.State"))

counts <- both %>% group_by(Vendor, Address.State) %>% 
  summarise(Total_For_Sale=n(),
            Exo_For_Sale = length(USDA.Code[US.Nativity=="EXOTIC"]),
            Native_Other_For_Sale = length(USDA.Code[US.Nativity=="NATIVE" | US.Nativity=="OTHER" |
                                                       US.Nativity=="BOTH"]),
            NW_For_Sale = length(USDA.Code[USDA.=="Y"]),
            SL_For_Sale = length(USDA.Code[State.Listed=="Y"]),
            Sold_In_State = length(USDA.Code[Sold.In.State=="Y"]))
head(counts)

# Bind counts back to master new so that all info is in the same place
master_done <- left_join(master_new2, counts %>% rename(Address_St=Address.State),
                         by=c("Vendor", "Address_St"))
str(master_done)
master_done$Vendor <- as.factor(master_done$Vendor)
str(master_done)

# Drop rogue vendor (Izel)
master_done <- master_done %>% filter(!Vendor=="Izel") %>% droplevels()
str(master_done)

# export data
# write.csv(master_done, "master_vendor_list_GIS_3.4.2020.csv", row.names=F)

# update coordinates in data sheets
# locs <- read.csv(file.choose())

# new.clim <- merge(data, locs[,1:5], by=c("Vendor", "Address"), all.x=T)
# write.csv(new.clim, "test.csv", row.names=F)

# Add column for number of sale records per state?
head(gog)
both <- rbind(gog %>% select(USDA.Code, Sold.In.State, State.Listed, USDA.Fed.Listed, Vendor, Vendor.Type,
                             Address.State),
              pio %>% select(USDA.Code, Sold.In.State, State.Listed, USDA.Fed.Listed, Vendor, Vendor.Type,
                             Address.State)) %>% distinct() %>% droplevels()
head(both)
both 
sales <- data.frame(table(both$Address.State)) %>% rename(state=Var1, sales=Freq)

setwd("C:/Users/ebeaury/Box/Invaders for Sale/final database results")
list <- read.csv("UPDATED_state_lists_2019.csv")
head(list)
species <- data.frame(table(list$state)) %>% rename(state=Var1, species=Freq)

counts <- left_join(sales, species, by="state")
counts$species <- counts$species %>% replace_na(0)
counts_nounk <- counts %>% filter(!state=="UNKNOWN")
ggplot(counts_nounk, aes(x=species, y=sales)) + geom_point() + geom_smooth()

lm1 <- glm(log(sales) ~ species, data=counts_nounk)
summary(lm1)
hist(log(counts_nounk$sales))
hist(counts_nounk$sales)
mean(counts_nounk$sales)
var(counts_nounk$sales)
?var

mean(log(counts_nounk$sales))
var(log(counts_nounk$sales))

library(MASS)
lm2 <- glm.nb(sales ~ species, data=counts_nounk)
summary(lm2)

# write.csv(counts, "sales_by_state.csv", row.names = F)

# Number of vendors per state
str(both)
both.ven <- both %>% select(Vendor, Address.State) %>% distinct() %>% droplevels()
counts <- data.frame(table(both.ven$Address.State)) %>% rename(state=Var1, no_vens=Freq)
write.csv(counts, "count_vens_by_state.csv", row.names=F)
sum(counts$no_vens)
