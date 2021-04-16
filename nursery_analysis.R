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

## Load data

# Species datasheets
pio <- read.csv("species.list.pio.csv")
google <- read.csv("species.list.google.csv")

# Vendor datasheets
vens.pio <- read.csv("vendors.pio.csv")
vens.gog <- read.csv("vendors.google.csv")

str(vens.pio)
str(vens.gog)

# Make sure there aren't any duplicate entries
vens.pio <- unique(vens.pio)
vens.gog <- unique(vens.gog)

## How many vendors are there and of what kind (nursery vs. non-nursery)
# Make a new dataframe with variables of interest
gogs <- unique(vens.gog %>% select(Vendor, Vendor.Type))
ugh <- data.frame(table(gogs$Vendor)) # Double check for duplicate values

# Count number of unique vendors of each class
table(gogs$Vendor.Type)

# Repeat for PIO
pios <- unique(vens.pio %>% select(Vendor, Nursery.type))
table(pios$Nursery.type)

# Make a dataframe to count which vendor sells retail, wholesale or both
counts <- data.frame(table(pios))
counts <- pivot_wider(counts, names_from = Nursery.type, values_from = Freq)
str(counts)
counts$total <- counts$both + counts$retail + counts$wholesale

# How many nurseries are listed as both retail and wholesale?
length(counts$total[counts$total > 1]) 
  # 19 vendors sell both retail + nursery
# How many only retail?
nrow(counts %>% filter(retail==1 & total==1)) 
# How many only wholesale?
nrow(counts %>% filter(wholesale==1 & total==1))

## Where are the vendors located?
# First, simplify the dataframe to the columns of interest and filter for nurseries only
locs <- unique(vens.gog %>% select(Vendor, Vendor.Type, Address.State)) %>% droplevels()
sort(table(locs$Address.State))
table(locs$Address.State, locs$Vendor.Type)
# How many non-nurseries could we not find location information for?


# Plot the frequencies
amounts <- data.frame(table(locs$Address.State, locs$Vendor.Type))
ggplot(amounts, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity") + 
  xlab("State") + ylab("Number of Google vendors")

# Repeat for PIO
## First simplify dataframe
locs.pio <- unique(vens.pio %>% select(Vendor, Address, Address.State))
sort(table(locs.pio$Address.State))
# Plot!
amounts <- data.frame(table(locs.pio$Address.State))
ggplot(amounts, aes(x=Var1, y=Freq)) + geom_bar(stat="identity") + 
  xlab("State") + ylab("Number of PIO nurseries")

## For sale habits in google database

# What portion of the database is by nurseries vs. non-nurseries?
table(vens.gog$Vendor.Type) # Count = number of for sale listings by one or ther other

# Are vendors selling plants/seeds/both?
table(vens.gog$Organism.Type, vens.gog$Vendor.Type)

# How many species does each vendor sell?
# First, count of number of times vendor shows up in database
freqs <- data.frame(table(vens.gog$Vendor, vens.gog$Vendor.Type))
# Drop 0s
freqs <- freqs[!freqs$Freq==0,]
# On average, how many species do nurseries sell? Does it differ for nurseries vs. non-nurseries
mean(freqs$Freq)
tapply(freqs$Freq, freqs$Var2, FUN=mean) # Looks like non-nurseries often carry more species
# How many species do most vendors sell?
sort(table(freqs$Freq)) # 1 is the most common number, most vendors sell only one species

# What are the frequencies of the different restrictions?
# Firt, filter to only look at nurseries
locs <- vens.gog %>% filter(Vendor.Type=="NURSERY")
# Count the number of times each restriction type comes up
sort(table(locs$Restriction.Type, exclude=NULL))

# What species are in the database most often (highest counts of vendors)?
sort(table(vens.gog$USDA.Code))
sort(table(vens.gog$USDA.Code[vens.gog$Vendor.Type=="NURSERY"]))
sort(table(vens.gog$USDA.Code[vens.gog$Vendor.Type=="NON-NURSERY"]))
vens.gog2 <- vens.gog %>% select(USDA.Code, Vendor) %>% distinct
sort(table(vens.gog2$Vendor))

# On average, how many times is species sold?
mean(table(table(vens.gog$USDA.Code)))

# Where are the shipping restrictions most often to?
# First, make a dataframe of the column with the locations
res <- data.frame(vens.gog %>% select(Restriction.Location))
# Drop NAs
res <- na.omit(res)
res <- droplevels(res)
res$Restriction.Location <- as.character(res$Restriction.Location)

# Reshape dataframe to separate the rows that have multiple states (e.g., AL, AK)
res <- strsplit(res$Restriction.Location, split=",")
res2 <- data.frame(unlist(res))
# Make a dataframe with the counts of how many times each state is reported
counts <- data.frame((table(res2$unlist.res.)))
# Open up counts (top right, click on counts) to look at frequencies

## Pio sale habits

# What portion of sales are retail vs. wholesale?
table(vens.pio$Nursery.type)

# On average, how many species does a vendor sell? 
mean(table(vens.pio$Vendor))
# How many species do most vendors sell?
sort(table(table(vens.pio$Vendor))) # 1 is most common number
# How many species does each vendor sell? Which is most common?
counts <- data.frame(table(vens.pio$Vendor))
# Open up counts (top right, click on counts) to look at frequencies

# What are the most common vendors?

# First, make a dataframe of counts of vendors
freqs <- data.frame(table(vens.pio$Vendor, vens.pio$Nursery.type))
# Drop zeros
freqs <- freqs[!freqs$Freq==0,]
# Look at average times a vendor shows up for different nursery types
tapply(freqs$Freq, freqs$Var2, FUN=mean)
sort(table(freqs$Freq)) # Most people still selling only one species, average driven by large sellers

# What are the most common species?

# First, make a dataframe of counts of species
df <- data.frame((table(vens.gog$USDA.Code, vens.gog$Vendor.Type)))
# Mean number of times a species is listed
mean(table(table(vens.gog$USDA.Code)))

# Which species most commonly sold
sort(table(vens.pio$USDA.Code))
sort(table(table(vens.pio$USDA.Code))) # But most species are only sold by 1 vendor
mean(table(table(vens.pio$USDA.Code))) # Mean number of times a species is sold

# Plot!
hist(table(vens.pio$USDA.Code),
     main="Distribution of number of times a species is sold",
     xlab="Number of vendors selling a species") # Most species are seld by a few vendors

# Combine databases and look at sales
# First, select common variables across data
gog <- vens.gog %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State)
pio <- vens.pio %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State)
# Combine and drop duplicates
both <- rbind(gog, pio)
both <- unique(both)
str(both)

# Portion of sales by types of vendors
table(both$Vendor.Type)
df <- data.frame(table(both$Vendor))
# Average number of species sold per vendor
mean(table(both$Vendor))
# Average number of vendors selling a species
mean(table(both$USDA.Code))

# Plot
type <- c("Nursery", "E-commerce")
numbers <- c(15097, 2294)
pie(numbers, type)

# How many species and unique vendors? (factor levels of each variable)
str(both)
both.vens <- both %>% select(Vendor, Vendor.Type) %>% distinct() %>% droplevels()
table(both.vens$Vendor.Type)

# Where are the vendors? 
# First, select variables of interest and filter to nurseries only
locs <- both %>% select(Vendor, Vendor.Type, Address.State)
locs <- unique(locs)
# How many in each state?
sort(table(locs$Address.State))
length(table(locs$Address.State))
# What are the most common vendors? (dataframe of counts of vendors)
counts.vens <- data.frame(table(both$Vendor)) # Open (top right) to see frequencies
hist(counts.vens$Freq)
# What are the most common species? (dataframe of counts of species)
counts.species <- data.frame(table(both$USDA.Code)) # Open (top right) to see frequencies

# What are the most commonly sold species by the different vendor types?
vendor.types <- both %>% select(USDA.Code, Vendor.Type)
counts.types <- data.frame(table(vendor.types$USDA.Code, vendor.types$Vendor.Type)) # Open (top right) to see frequencies


# Sale habits for exotic species only

# First, subset data to only show sales of exotix species
  # Pull native status from the species datasheet (google)
  # Join to vendor list (left_join)
exos <- unique(google %>% select(USDA.Code, US.Nativity))
head(exos)
new.vens.pio <- left_join(vens.pio, exos, by="USDA.Code") %>% filter(US.Nativity=="EXOTIC") %>%
  droplevels()
new.vens.pio$USDA.Code <- as.factor(new.vens.pio$USDA.Code)

# Look at how many nurseries there are of different types
str(new.vens.pio)
new.vens.pio2 <- new.vens.pio %>% select(Vendor, Nursery.type) %>% distinct() %>% droplevels()
table(new.vens.pio2$Nursery.type)

# Repeat for google
new.vens.gog <- left_join(vens.gog, exos, by="USDA.Code") %>% filter(US.Nativity=="EXOTIC") %>%
  droplevels()
new.vens.gog$USDA.Code <- as.factor(new.vens.gog$USDA.Code)

str(new.vens.gog)
new.vens.gog2 <- new.vens.gog %>% select(Vendor, Vendor.Type) %>% distinct()
table(new.vens.gog2$Vendor.Type)

# Look at exotic species in both datasets

# First, combine data
gog.vens.3 <- new.vens.gog %>% select(USDA.Code, Vendor, Vendor.Type)
pio.vens.3 <- new.vens.pio %>% select(USDA.Code, Vendor, Vendor.Type)
both.exo <- rbind(gog.vens.3, pio.vens.3) %>% distinct()

# Look at structure of data to see how many unique vendors there are
str(both.exo)

# Next, look at nurservy vs. non-nursery sales
both.exo2 <- both.exo %>% select(Vendor, Vendor.Type) %>% distinct()
table(both.exo$Vendor.Type)

# Where are exotic species sold?
str(new.vens.pio) # Look at Address.State - sold in all lower 48 states
new.vens.pio2 <- new.vens.pio %>% select(Vendor, Address.State) %>% distinct()
sort(table(new.vens.pio2$Address.State, exclude=NULL))

# Repeat for google
str(new.vens.gog) # Look at Address.State - sold in 49 states?
# Look at that the factor levels are to figure out what the 49th thing is
levels(new.vens.gog$Address.State) # Oh - for unknown address states

# Look at counts of vendors in each state
new.vens.gog2 <- new.vens.gog %>% select(Vendor, Address.State) %>% distinct() %>% droplevels()
sort(table(new.vens.gog2$Address.State, exclude=NULL))

# Combine data for exotics to look at vendor locations
both.locs <- rbind(new.vens.gog2, new.vens.pio2) %>% distinct()
sort(table(both.locs$Address.State))

# Exotic species sale habits

# What nursery types sell these species?
table(new.vens.pio$Nursery.type)

new.vens.pio2 <- new.vens.pio %>% select(USDA.Code, Vendor) %>% distinct() %>% droplevels()
mean(table(new.vens.pio2$Vendor)) # Avergage number of species each vendor sells
hist(table(new.vens.pio2$Vendor), 
     main="", xlab="Number of species each vendor sells",
     ylab="Number of vendors") # Distribution of number of species each vendor sells - most vendors sell a few species

# Who are the most common vendors of exotic species in PIO?
freqs <- data.frame(table(new.vens.pio$Vendor, new.vens.pio$Nursery.type))
freqs <- freqs[!freqs$Freq==0,] # Open (top right) to see counts
# Average number of species each vendor sells by vendor type
tapply(freqs$Freq, freqs$Var2, FUN=mean)
# Most vendors sell 1 species
sort(table(freqs$Freq))

# Who is the most common PIO vendor of exotics?
sort(table(new.vens.pio2$Vendor))
# What is the most commonly sold exotic species?
sort(table(new.vens.pio2$USDA.Code))
# On average, how many times is a species sold?
mean(table(new.vens.pio2$USDA.Code))

# Repeat sales breakdowns for google

# What portion of exotic sales are nursery vs non-nursery?
table(new.vens.gog$Vendor.Type)
# What organism type are they selling?
table(new.vens.gog$Organism.Type, new.vens.gog$Vendor.Type)
# How many species does each vendor sell?
freqs <- data.frame(table(new.vens.gog$Vendor, new.vens.gog$Vendor.Type)) # open to see counts
freqs <- freqs[!freqs$Freq==0,]
# Average number of species each vendor sells, overall and by vendor type
mean(freqs$Freq)
tapply(freqs$Freq, freqs$Var2, FUN=mean)
sort(table(freqs$Freq)) # Most people sell one species

# Who are the most common vendors in google?
new.vens.gog2 <- new.vens.gog %>% select(USDA.Code, Vendor, Vendor.Type) %>% distinct()
sort(table(new.vens.gog2$Vendor))

# Google restrictions

# How many sales have a restriction? (NAs = non-nursery records)
table(new.vens.gog$Restriction, exclude=NULL)
# Count of different kinds of restructons
table(new.vens.gog$Restriction.Type)
# Where are the shipping restriction locations to?
res <- data.frame(new.vens.gog %>% select(Restriction.Location)) %>% na.omit() %>% droplevels()
res$Restriction.Location <- as.character(res$Restriction.Location)
res <- strsplit(res$Restriction.Location, split=",")
res2 <- data.frame(unlist(res))

# Look at counts for different locations:
counts.exo.restrictions <- data.frame((table(res2$unlist.res.))) # Open top right to see counts

# Most commonly sold exotic species in google
sort(table(new.vens.gog2$USDA.Code))

# Exoticsale habits for both datasets
both <- rbind(new.vens.gog %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State),
              new.vens.pio %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State)) %>%
  distinct() %>% droplevels()

# Portion nursery vs. non-nursery
table(both$Vendor.Type)
# Mean number of species each vendor sells
mean(table(both$Vendor))
# Mean number of vendors that sell each species
both2 <- both %>% select(USDA.Code, Vendor, Vendor.Type) %>% distinct() %>% droplevels()
mean(table(both2$USDA.Code))

# Common vendors and species
sort(table(both2$USDA.Code))
counts.exo.vens <- data.frame(table(both2$Vendor))

#
#
#

# Let's clear the directory and start with clean data
rm(list=ls())
vens.pio <- read.csv("vendors.pio.csv")
vens.gog <- read.csv("vendors.google.csv")

#
# Noxious weed sale habbits
#

# First, subset data to only look at noxious weeds
pio.nw <- vens.pio %>% filter(USDA.Fed.Listed=="Y") %>% droplevels()
# Double check this is data for 12 species (factor levels of USDA.Code), and how many unique vendors?
str(pio.nw)

# What noxious weeds were found for sale?
levels(pio.nw$USDA.Code)
# Who is selling them?
nwv <- pio.nw %>% select(Vendor, Nursery.type) %>% distinct()
table(nwv$Nursery.type)

# Repeat for google
gog.nw <- vens.gog %>% filter(USDA.Fed.Listed=="Y") %>% droplevels()
# Double check this is data for 14 species (factor levels of USDA.Code), and how many unique vendors?
str(gog.nw)

# What types of vendors sell noxious weeds?
nwg <- gog.nw %>% select(Vendor, Vendor.Type) %>% distinct()
table(nwg$Vendor.Type)

# Noxious weed sales across both datasets

# First, combine data
both.nw <- rbind(pio.nw %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State), 
                 gog.nw %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State)) %>%
  distinct()
# How many species for sale by how many vendors? (look at factor levels)
str(both.nw)

# Look at what kinds of vendors
both.nw2 <- both.nw %>% select(Vendor, Vendor.Type) %>% distinct()
table(both.nw2$Vendor.Type)

# Where are the noxious weeds sold?
str(pio.nw) # Look at levels for how many states vendors are in
levels(pio.nw$Address.State) # What are those states?
sort(table(pio.nw$Address.State)) # How frequent are vendors in each state?

# Repeat for google
str(gog.nw) # Look at levels for how many states vendors are in
levels(gog.nw$Address.State) # What are those states?
sort(table(gog.nw$Address.State)) # How frequent are vendors in each state?

# Repeat for combined data
str(both.nw) # Look at levels for how many states vendors are in
sort(table(both.nw$Address.State)) # How frequent are vendors in each state?
levels(both.nw$Address.State)

#
# Sale habits for noxious weeds
#

# What vendor types and who are the vendors?
table(pio.nw$Nursery.type)

pio.nw2 <- pio.nw %>% select(USDA.Code, Vendor) %>% distinct() %>% droplevels()
sort(table(pio.nw2$Vendor)) # Number of species sold by each vendor (only two sell 2 noxious weeds)
sort(table(pio.nw2$USDA.Code)) # IMCY is most commonly sold noxious weed

# Repeat for google - vendor types and organisms sold
table(gog.nw$Vendor.Type)
table(gog.nw$Organism.Type, gog.nw$Vendor.Type)

gog.nw2 <- gog.nw %>% select(USDA.Code, Vendor, Vendor.Type) %>% distinct() %>% droplevels()
sort(table(gog.nw2$Vendor)) # Number of species sold by each vendor
sort(table(gog.nw2$USDA.Code)) # Again, IMCY most commonly sold species

# Are there restrictions on noxious weed sales?
table(gog.nw$Restriction, exclude=NULL)
# What types?
table(gog.nw$Restriction.Type) # Only one labeled...
# Where are the restriction locations
res <- data.frame(gog.nw %>% select(Restriction.Location)) %>% na.omit() %>% droplevels()
# ^ Open dataframe to see

# Sales of noxious weeds in both datasets
both.nw <- rbind(gog.nw %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State),
                 pio.nw %>% select(USDA.Code, Vendor, Vendor.Type, Nursery.type, Address.State)) %>%
  distinct() %>% droplevels()
# What types of vendors and how often?
table(both.nw$Vendor.Type)
sort(table(both.nw$Vendor)) # Ebay most commonly selling noxious weeds
sort(table(both.nw$USDA.Code)) # IMCY most commonly sold

# Where is IMCY sold?
imcy <- both.nw %>% filter(USDA.Code=="IMCY") %>% droplevels()
head(imcy)
levels(imcy$Address.State)

# Are there vendors that only sell native species?
google <- read.csv("species.list.google.csv")
nats <- unique(google %>% select(USDA.Code, US.Nativity))
new.vens.pio <- left_join(vens.pio, nats, by="USDA.Code") %>% select(USDA.Code, Vendor, US.Nativity) %>% 
  distinct() %>% droplevels()
head(new.vens.pio)
str(new.vens.pio)

new.vens.gog <- left_join(vens.gog, nats, by="USDA.Code") %>% select(USDA.Code, Vendor, US.Nativity) %>% 
  distinct() %>% droplevels()
head(new.vens.gog)

both <- rbind(new.vens.gog,new.vens.pio) %>% distinct() %>% droplevels()
counts <- data.frame(table(both$Vendor, both$US.Nativity))
counts <- data.frame(counts %>% pivot_wider(names_from = Var2, values_from = Freq))
table(counts$NATIVE)
print(counts %>% filter(NATIVE > "0" & EXOTIC == 0))
exos <- counts %>% filter(EXOTIC > 0 & NATIVE == 0 & BOTH == 0 & OTHER == 0)

str(counts)
counts <- counts %>% select(-total)

# pie chart?
labels <- c("All records")

# Subset data for climate change component
# First, pull vendor and code information
both.vens <- rbind(vens.pio %>% select(Vendor, USDA.Code, Sold.In.State, State.Listed),
                   vens.gog %>% select(Vendor, USDA.Code, Sold.In.State, State.Listed)) %>%
  distinct() %>% droplevels()
head(both.vens)
str(both.vens)

# Second, bind in info from species sheet (GH, Nativity)
both.vens2 <- left_join(both.vens, google %>% select(USDA.Code, Growth.Habit, US.Nativity, USDA.) %>% distinct())
head(both.vens2)

# Third, bind in location info from spatial data
locs <- read.csv(file.choose())
head(locs)
both.vens3 <- left_join(both.vens2, locs %>% select(Vendor, Address, Address_St, Latitude, Longitude) %>% 
                          distinct(), by="Vendor") %>% distinct()
table(both.vens2$USDA.Code)
table(both.vens3$USDA.Code)

print(both.vens3 %>% filter(Vendor=="Zone 9 Tropicals"))
both.vens4 <- both.vens3 %>% select(Vendor, Address, USDA.Code, Growth.Habit, US.Nativity,
                                   Sold.In.State, State.Listed, USDA., Address_St, Latitude, Longitude) %>%
  rename(USDACode = USDA.Code, GrowthHabit=Growth.Habit, USNativity=US.Nativity,SoldInState=Sold.In.State,
         StateListed=State.Listed, USDAFedListed=USDA., AddressSt=Address_St)
head(both.vens4)

str(both.vens4)
both.vens5 <- both.vens4 %>% filter(!AddressSt=="UNKNOWN") %>% distinct() %>% droplevels()

# Drop natives
table(both.vens5$USNativity)
both.vens6 <- both.vens5 %>% filter(!USNativity=="NATIVE")
getwd()
write.csv(both.vens6, "vendors.clim.csv", row.names=F)

