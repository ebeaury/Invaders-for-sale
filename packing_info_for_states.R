rm(list=ls())

#load packages
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Eve/Box/Invaders for Sale/final database results")
setwd("/Users/ebeaury/Box/Invaders for Sale/final database results")


# Three components of information:
  # list of nurseries selling their own state invasives in state
  # list of nurseries selling their state listed species (even if out of state) 
  # list of nurseries selling other state invasives in state

# Load govt lists so we can subset by state listed for each state
govt <- read.csv("UPDATED_state_lists_2019.csv")
head(govt)
# Subset to just state regulated
state_govt <- govt %>% filter(FNW.status == "N")
fed_govt <- govt %>% filter(FNW.status=="Y")

# Load nursery lists and combine
gog <- read.csv("vendors.google.csv")
pio <- read.csv("vendors.pio.csv")
head(gog)
head(pio)
both <- rbind(gog %>% select(USDA.Code, Sold.In.State, State.Listed, USDA.Fed.Listed,
                             SN.used.by.vendor, CN.used.by.vendor, Link, Vendor, Vendor.Type, Address,
                             Address.State),
              pio %>% select(USDA.Code, Sold.In.State, State.Listed, USDA.Fed.Listed,
                             SN.used.by.vendor, CN.used.by.vendor, Link, Vendor, Vendor.Type, Address,
                             Address.State)) %>% distinct() %>% droplevels()

# Set up loop to export data for each state
setwd("/Users/ebeaury/Box/Invaders for Sale/final database results/state regulated data")
states <- unique(govt$state)
# states <- "MA"

for(S in states){
  # Pull nurseries located in the state
  target_vendors <- both %>% filter(Address.State==S)
  
  # Identify which species are regulated within the state
  target_reg <- govt %>% filter(state==S) %>% droplevels()
  target_vendors <- target_vendors %>%
    mutate(reg.in.state = ifelse(USDA.Code %in% target_reg$USDA.code, "Y", "N"))
  
  # Identify what other species are regulated (ie, in other states)
  target_vendors <- target_vendors %>%
    mutate(reg.elsewhere = ifelse(USDA.Code %in% state_govt$USDA.code, "Y", "N"))
 
  # Identify which species are federally regulated
  target_vendors <- target_vendors %>%
    mutate(fed.reg = ifelse(USDA.Code %in% fed_govt$USDA.code, "Y", "N"))
  
  # Add variable to govt lists that shows where each species is listed
  govt_sub <- govt %>% group_by(USDA.code) %>% mutate(states=paste(state, collapse = ",")) %>%
    rename(USDA.Code=USDA.code)
  target_vendors <- left_join(target_vendors, govt_sub %>% select(USDA.Code, states),by="USDA.Code") %>% 
    rename(States.where.listed=states) %>% distinct()
  
  # Rename the columns
  target_data <- target_vendors %>% 
    select(-c(Vendor.Type, Sold.In.State, State.Listed, USDA.Fed.Listed)) %>%
    rename('USDA PLANTS Code'= USDA.Code, 'Scientific name used by vendor' = SN.used.by.vendor, 
           'Common name used by vendor' = CN.used.by.vendor, 'Address state' = Address.State,
           'State regulated' = reg.elsewhere, 'Federally regulated' = fed.reg, 
           'Other states where regulated' = States.where.listed)
  colnames(target_data)[8] <- paste0(S, " regulated")
  write.csv(target_data, paste0(S, "_OrnamentalInvaders_2020.csv"), row.names=F)
  
}

#
#
#
#

# Pulling ornamental invaders for NE
govt <- read.csv("UPDATED_state_lists_2019.csv")

# fitler for NE
states <- c("CT", "MA", "ME", "VT", "RI", "NH", "NY")
govt_NE <- govt %>% filter(state %in% states)
govt_NE <- govt_NE %>% group_by(USDA.code) %>%
  mutate(states.where.reg = paste(state, collapse = ",")) %>%
  select(USDA.code, states.where.reg) %>% distinct() %>% rename(USDA.Code = USDA.code)

# Subset invaders for sale data to NE and select NE regulated species
both_NE <- both %>% filter(Address.State %in% states, USDA.Code %in% govt_NE$USDA.code)

# join regulations
both_NE <- left_join(both_NE, govt_NE)
length(unique(both_NE$USDA.Code))
write.csv(both_NE, "NE_regulated_ornamentals.csv", row.names=F)

#
#
#
#
#
#
#
#
#
#
#

#
#
#

#
#
#
#
#
#
#
#
#
#
#

# 
#  'Other states where regulated' = States.where.listed

for(S in states){
  # Select species listed in focal state
  target_reg <- govt %>% filter(state==S) %>% droplevels()
  
  # Identify target state's listed species
  both$target.list <- ifelse(both$USDA.Code %in% target_reg$USDA.code, "Y", "N")

  # 1) list of nurseries selling their state listed species (even if out of state)
  target_in_state <- both %>% filter(target.list=="Y") %>% 
    select(USDA.Code,SN.used.by.vendor, CN.used.by.vendor, Link, Vendor, Vendor.Type, Address,
           Address.State) %>%
    rename(Scientific.Name = SN.used.by.vendor, Common.Name=CN.used.by.vendor) %>% distinct()
  glimpse(target_in_state)

  # 2) list of nurseries selling their own state invasives in state
  target_sold_in_state <- both %>% filter(Sold.In.State=="Y" & Address.State== S)
  
  # 3) list of nurseries selling other state invasives in state
  target_all_vens <- both %>% filter(Address.State==S, State.Listed =="Y") %>%
    select(USDA.Code,SN.used.by.vendor, CN.used.by.vendor, Link, Vendor, Vendor.Type, Address,
           Address.State) %>%
    rename(Scientific.Name = SN.used.by.vendor, Common.Name=CN.used.by.vendor) %>% droplevels()
  # Add variable to govt lists that shows where each species is listed
  govt_sub <- govt %>% group_by(USDA.code) %>% mutate(states=paste(state, collapse = ",")) %>%
    rename(USDA.Code=USDA.code)
  target_all_vens <- left_join(target_all_vens, govt_sub %>% select(USDA.Code, states),by="USDA.Code") %>% 
    rename(States.where.listed=states) %>% distinct()

  # 4) Are any noxious weeds sold in state?
  nw_in_target <- both %>% filter(Address.State==S & USDA.Fed.Listed=="Y") # sometimes this will be empty
  nw_in_target <- nw_in_target %>% select(-c(Sold.In.State, State.Listed, target.list)) %>%
    rename('Federally regulated' = USDA.Fed.Listed, Scientific.Name = SN.used.by.vendor,
           Common.Name = CN.used.by.vendor) %>%
    mutate("States.where.listed"=NA)
  
  # Package data
  target_data <- rbind(nw_in_target, target_all_vens %>% mutate('Federally regulated' = "N")) 
  target_data <- target_data %>% 
    rename('USDA PLANTS Code'= USDA.Code, 'Scientific name used by vendor' = Scientific.Name, 
           'Common name used by vendor' = Common.Name, 'Address state' = Address.State,
           'Other states where regulated' = States.where.listed) %>%
    select(-Vendor.Type)
  write.csv(target_data, paste0(S, "_OrnamentalInvaders_2020.csv"), row.names=F)
}

#
#
#
#
#
#
#
#
#
####
#######
######## ARCHIVED BELOW ################
######
####
####
#
#
#
#
#
#
#
#
#

# Select species listed in focal state
target_reg <- govt %>% filter(state=="MA") %>% droplevels()

# Identify target state's listed species
both$target.list <- ifelse(both$USDA.Code %in% target_reg$USDA.code, "Y", "N")
head(both)

# 1) list of nurseries selling their state listed species (even if out of state)
target_in_state <- both %>% filter(target.list=="Y") %>% 
  select(USDA.Code,SN.used.by.vendor, CN.used.by.vendor, Link, Vendor, Vendor.Type, Address,
         Address.State) %>%
  rename(Scientific.Name = SN.used.by.vendor, Common.Name=CN.used.by.vendor) %>% distinct()
glimpse(target_in_state)
# write.csv(target_in_state, "MA_state_listed_species.csv", row.names=F)

# 2) list of nurseries selling their own state invasives in state
target_sold_in_state <- both %>% filter(Sold.In.State=="Y" & Address.State=="MA")

# 3) list of nurseries selling other state invasives in state
target_all_vens <- both %>% filter(Address.State=="MA", State.Listed =="Y") %>%
  select(USDA.Code,SN.used.by.vendor, CN.used.by.vendor, Link, Vendor, Vendor.Type, Address,
         Address.State) %>%
  rename(Scientific.Name = SN.used.by.vendor, Common.Name=CN.used.by.vendor) %>% droplevels()
# Add variable to govt lists that shows where each species is listed
govt <- govt %>% group_by(USDA.code) %>% mutate(states=paste(state, collapse = ",")) %>%
  rename(USDA.Code=USDA.code)
target_all_vens <- left_join(target_all_vens, govt %>% select(USDA.Code, states),by="USDA.Code") %>% 
  rename(States.where.listed=states) %>% distinct()
write.csv(MA_all_vens, "All_state_listed_NH.csv", row.names=F)

# 4) Are any noxious weeds sold in state?
nw <- both %>% filter(Address.State=="MA" & USDA.Fed.Listed=="Y") # sometimes this will be empty

# Combine state reg and nw
colnames(nw)
colnames(MA_sold_in_state)
target_data <- rbind(nw, target_all_vens) 
setwd("/Users/ebeaury/Box/Invaders for Sale/final database results/state regulated data")
write.csv(MN_data, "InvadersForSale_MN_EBeaury_2020.csv", row.names=F)

glimpse(govt)
govt %>% filter(USDA.code=="STPA")
