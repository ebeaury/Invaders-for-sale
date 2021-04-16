 rm(list=ls()) # Clear directory

### Analyzing the species lists ###
  # Text preceded with a '#' is an annotation
  # This analysis is very simple, mostly summary statistics (counts, means, etc.)
  # The main thing to be careful of is excluding duplicate entries, especially when combining databases
  # The common functions are:
    # making tables of counts, kind of like the pivot table function in excel
    ?table() # see explanation bottom right
    # unique - drops any duplicate values
    ?unique()
    # droplevels - drops any unused columns/rows in a dataframe, typically those filled with NA
    ?droplevels()
    # subsetting using brackets [], the format goes...
      # data[rows you want to keep,columns you want to keep]
        # for example - data[4:10,6] - this would select rows 4 through 10 in column 6
      # you can also subset using column names
        # for example - data[4:10,data$USDA.code] - this would select rows 4 through 10 in the column titled 'USDA.Code'
      # you can also select rows based on the content of the row
        # for example data[data$for.sale=="Y",data$for.sale] - this would select only rows containing "Y" within the 'For Sale' column
  # If you want details on any of the functions, put a ? in front of the function for a description (bottom right)

# Load packages - these are packages of R functions for data subsetting and analysis
# install.packages("dplyr") # You only need to install this once
# install.packages("tidyr")
library(dplyr) # You need to run this code every time you open a new RStudio session
library(tidyr)

# Tell R where to find your data files
setwd("C:/Users/Eve/Box/Invaders for Sale/final database results")
setwd("/Users/ebeaury/Box/Invaders for Sale/final database results")

#
#
#

# Load data
pio <- read.csv("species.list.pio.csv")
google <- read.csv("species.list.google.csv")

# Look at the structure of the files to see how R interprets each variable and to make sure we loaded everything correctly
str(pio)
str(google)

# How many species are for sale in the individual databases?
  ## Make a new dataframe with a subset of the data with just USDA Codes and the 'For sale' column.
  fors.pio <- pio %>% select(USDA.Code, For.Sale.)
  ## Remove duplicate values so that we don't double count synonymous USDA codes
  fors.pio <- unique(fors.pio)
  no.dups <- data.frame(table(fors.pio$USDA.Code)) # Double check that no USDA codes where double counted
  
  # Count how many Y, N, UNKNOWN values (NA) are in the 'For Sale' column
  table(fors.pio$For.Sale., exclude=NULL) 
  
  ## Repeat for google
  fors.gog <- google %>% select(USDA.Code, For.Sale.)
  fors.gog <- unique(fors.gog)
  no.dups <- data.frame(table(fors.gog$USDA.Code)) #double check for double counts
  
  # Get the counts
  table(fors.gog$For.Sale., exclude=NULL)

# How many species are for sale in the combined database?
  ## Filter dataset to show only species found for sale
  fors.pio.y <- fors.pio %>% filter(For.Sale.=="Y")
  fors.pio.y <- na.omit(fors.pio.y) # Drop anything not for sale
  fors.pio.y <- droplevels(fors.pio.y) # Drop anything not for sale

  fors.gog.y <- fors.gog %>% filter(For.Sale.=="Y")
  fors.gog.y <- droplevels(fors.gog.y)
  
  ## Combine databases
  both <- rbind(fors.gog.y, fors.pio.y) #1331
  both <- unique(both) # Remove any duplicates (when pio and google found the same nurseries)

  # Number of rows in dataframe = number of species found for sale
  nrow(both)
  1285-nrow(both) # number searched - number found for sale = number not found for sale
  nrow(both) / 1285
# How many exotic species are found for sale?
  ## Subset data to include nativity column
  fors.pio.exo <- pio %>% select(USDA.Code, For.Sale., US.Nativity)
  fors.pio.exo <- unique(fors.pio.exo) # Drop duplicates
  no.dups <- data.frame(table(fors.pio.exo$USDA.Code)) # Double check for double counts
  
  # Count how many of each type were searched
  table(fors.pio.exo$US.Nativity, exclude=NULL) 
  # Count how many of each type were found for sale
  table(fors.pio.exo$US.Nativity, fors.pio.exo$For.Sale., exclude=NULL) # How many for sale

  ## Repeat for google
  fors.gog.exo <- google %>% select(USDA.Code, For.Sale., US.Nativity)
  fors.gog.exo <- unique(fors.gog.exo)
  no.dups <- data.frame(table(fors.gog.exo$USDA.Code)) #double check for double counts

  # Count how many of each type were searched (should be same as PIO)
  table(fors.gog.exo$US.Nativity, exclude=NULL)
  # Count how many of each type were found for sale
  table(fors.gog.exo$US.Nativity, fors.gog.exo$For.Sale., exclude=NULL)

  ## Combine databases
  # Subset data to select exotic species found for sale
  fors.pio.y2 <- pio %>% select(USDA.Code, For.Sale., US.Nativity) %>% 
    filter(For.Sale.=="Y" & US.Nativity=="EXOTIC")
  fors.pio.y2 <- na.omit(fors.pio.y2)
  fors.pio.y2 <- droplevels(unique(fors.pio.y2))

  fors.gog.y2 <- google %>% select(USDA.Code, For.Sale., US.Nativity) %>% 
    filter(For.Sale.=="Y" & US.Nativity=="EXOTIC")
  fors.gog.y2 <- na.omit(fors.gog.y2)
  fors.gog.y2 <- droplevels(unique(fors.gog.y2))

  # Combine databases
  both.exo <- rbind(fors.pio.y2,fors.gog.y2) 
  both.exo <- unique(both.exo)
  
  # Look for number of levels in USDA code column
  # This indicates the number of unique USDA codes found for sale
  str(both.exo) 
  table(both.exo$For.Sale., both.exo$US.Nativity) # Double check
  
## Now look at other nativities
  # First, combine data from google and pio
  both.all <- rbind(pio %>% select(USDA.Code, US.Nativity, For.Sale.) %>%
                      filter(For.Sale.=="Y"),
                    google %>% select(USDA.Code, US.Nativity, For.Sale.) %>%
                      filter(For.Sale.=="Y"))
  both.all <- (unique(both.all))
  no.dups <- data.frame(table(both.all$USDA.Code)) # Double check for double counts

  # Count the number for sale for other origins
  table(both.all$US.Nativity, exclude=NULL) 

# How many state listed species are for sale?
  ## Load list of government listed species
  states <- read.csv("UPDATED_state_lists_2019.csv")
  head(states) # Glance at the data
  
  ## Identify state listed species in species list
    # Create a new column called 'State.Listed' where we identify if it's state listed or not (Y/N)
    # If the USDA codes match between databases, record 'Y' for state listed, otherwise record 'N'
  pio$State.Listed <- ifelse(pio$USDA.Code %in% states$USDA.code, "Y", "N") 
  google$State.Listed <- ifelse(google$USDA.Code %in% states$USDA.code, "Y", "N")
  # Double check the column worked (should be last column in dataframe)
  head(pio)
  head(google)

  ## Subset data to look at only the state listed species that are for sale
  fors.pio.sl <- pio %>% select(USDA.Code, US.Nativity,For.Sale., State.Listed)
  fors.pio.sl <- unique(fors.pio.sl)
  fors.pio.sl <- droplevels(fors.pio.sl)
  no.dups <- data.frame(table(fors.pio.sl$USDA.Code)) # Double check for double counts
  
  # Count how many state listed species are there
  table(fors.pio.sl$State.Listed, exclude=NULL) 
  # Count how many are found for sale (Y in both columns = state listed and found for sale)
  table(fors.pio.sl$State.Listed, fors.pio.sl$For.Sale., exclude=NULL)

  ## Repeat for google
  fors.gog.sl <- google %>% select(USDA.Code, US.Nativity, For.Sale., State.Listed)
  fors.gog.sl <- unique(fors.gog.sl)
  fors.gog.sl <- droplevels(fors.gog.sl)
  no.dups <- data.frame(table(fors.gog.sl$USDA.Code)) #double check for double counts

  # Count how many state listed species are there (should match PIO)
  table(fors.gog.sl$State.Listed, exclude=NULL)
  # Count how many are found for sale (Y in both columns = state listed and found for sale)
  table(fors.gog.sl$State.Listed, fors.gog.sl$For.Sale., exclude=NULL)
  
  fors.pio.sl$State.Listed <- as.factor(fors.pio.sl$State.Listed) # Make sure columns are identified as factors so Y, N can be isolated
  fors.gog.sl$State.Listed <- as.factor(fors.gog.sl$State.Listed)

  ## Combine databases
  fors.pio.sl.y <- fors.pio.sl %>% filter(For.Sale.=="Y" & State.Listed=="Y")
  fors.gog.sl.y <- fors.gog.sl %>% filter(For.Sale.=="Y" & State.Listed=="Y")
  both.sl <- rbind(fors.pio.sl.y, fors.gog.sl.y)
  both.sl <- na.omit(both.sl)
  both.sl <- droplevels(both.sl)
  
  # Look for number of levels in USDA code column
  # This indicates the number of unique USDA codes that are state listed AND found for sale
  str(both.sl)
  # How many sl of each origin
  table(both.sl$US.Nativity)
  
# How many noxious weeds are found for sale?
  ## Subset data based on noxious weed status
  # 'USDA.' column indiciates noxious weed status (Y=noxious weed, N=not a noxious weed)
  fors.pio.nw <- pio %>% select(USDA.Code, USDA., For.Sale.)
  fors.pio.nw <- unique(fors.pio.nw)
  no.dups <- data.frame(table(fors.pio.nw$USDA.Code)) # Double check for double counts

  # Count how many noxious weeds there are
  table(fors.pio.nw$USDA., exclude=NULL)
  # Count how many are found for sale (Y in both columns)
  table(fors.pio.nw$USDA., fors.pio.nw$For.Sale., exclude=NULL)

  ## Repeat for google
  fors.gog.nw <- google %>% select(USDA.Code, USDA., For.Sale.)
  fors.gog.nw <- unique(fors.gog.nw)
  no.dups <- data.frame(table(fors.gog.nw$USDA.Code)) #double check for double counts

  # Count how many noxious weeds there are (should match PIO)
  table(fors.gog.nw$USDA., exclude=NULL)
  # Count how many are found for sale (Y in both columns)
  table(fors.gog.nw$USDA., fors.gog.nw$For.Sale., exclude=NULL)
  
  ## Combine databases, filter those that are found for sale
  fors.pio.nw.y <- fors.pio.nw %>% filter(For.Sale.=="Y")
  fors.gog.nw.y <- fors.gog.nw %>% filter(For.Sale.=="Y")
  
  both.nw <- rbind(fors.pio.nw.y,fors.gog.nw.y)
  both.nw <- na.omit(both.nw)
  both.nw <- unique(both.nw)
  both.nw <- droplevels(both.nw)
  # Count how many for sale?
  table(both.nw$For.Sale., both.nw$USDA.) 


# Of the species for sale, what are their growth habits (forb, grass, tree, etc.)?
  ## Subset data to include growth habit
  fors.pio <- pio %>% select(USDA.Code, For.Sale., Growth.Habit)
  fors.pio <- unique(fors.pio)
  no.dups <- data.frame(table(fors.pio$USDA.Code)) # Double check for double counts
  
  # Count how many of each growth habit are in database
  sort(table(fors.pio$Growth.Habit, exclude=NULL))
  # Count how many are for sale
  table(fors.pio$Growth.Habit, fors.pio$For.Sale., exclude=NULL) 

  ## Repeat for google
  fors.google <- google %>% select(USDA.Code, For.Sale., Growth.Habit)
  fors.google <- unique(fors.google)
  no.dups <- data.frame(table(fors.google$USDA.Code)) #double check for double counts

  # Count how many of each growth habit are in database
  table(fors.google$Growth.Habit, exclude=NULL)
  # Count how many are for sale
  table(fors.google$Growth.Habit, fors.google$For.Sale., exclude=NULL)
  
  ## Combine databases
  both.gh <- rbind(fors.pio, fors.google)
  table(both.gh$Growth.Habit)
  both.gh.Y <- both.gh %>% filter(For.Sale.=="Y")
  both.gh.Y <- unique(both.gh.Y)
  
  # Count of how many of both databases are found for sale (ignore other columns)
  table(both.gh.Y$Growth.Habit, both.gh.Y$For.Sale., exclude = NULL)

# How many origin for state listed
  gog.sl <- google %>% select(USDA.Code, US.Nativity, State.Listed, For.Sale.) %>% 
    filter(State.Listed=="Y") %>% distinct() %>% droplevels()
  pio.sl <- google %>% select(USDA.Code, US.Nativity, State.Listed, For.Sale.) %>% 
    filter(State.Listed=="Y") %>% distinct() %>% droplevels()
str(gog.sl)  
table(gog.sl$US.Nativity, exclude=NULL)

# Are the species originally introduced as ornamentals still for sale?
source <- read.csv("lehan_s1.csv")
head(source)

# Are all these species on our list?
df <- data.frame(table(source$USDA.PLANTS.CODE))
# Drop blanks
source <- source %>% filter(!USDA.PLANTS.CODE=="") %>% droplevels()
source$searched <- ifelse(source$USDA.PLANTS.CODE %in% google$USDA.Code, "Y", "N")
table(source$searched)

# Subset species that were introduced as ornamental, forage, or turfgrass
source_sub <- source %>% 
  filter(Intentional==1 & Intentional.Introduction.Type %in% c("Forage","forage crop",
            "Forage Crop","Forestry Planting","Medicinal, forage crop", "ornamental",
            "Ornamental", "Ornamental, Environmental Restoration Tool", "ornamental, medicinal",
            "Ornamental, Medicinal, Forage Crop", "put in wild seed mixes",
            "turfgrass", "aquarium use")) %>% droplevels()

levels(source_sub$Intentional.Introduction.Type)
str(source_sub$USDA.PLANTS.CODE) # Data for 510 species
nlevels(as.factor(source_sub$USDA.PLANTS.CODE))

# Filter out ones we didn't search
source_sub <- source_sub %>% filter(searched=="Y") %>% droplevels
str(source_sub)
print(source_sub %>% filter(Intentional.Introduction.Type=="turfgrass") %>% select(USDA.PLANTS.CODE))

# What species originally introduced as ornamentals are still for sale
source_sub$forsale <- ifelse(source_sub$USDA.PLANTS.CODE %in% both$USDA.Code, "Y", "N")
table(source_sub$forsale) # -1 in N for repeated code CYLO11
sort(table(source_sub$USDA.PLANTS.CODE))
360/43

# What are their intro dates?
dates <- read.csv("lehan_s2.csv")
head(dates)
str(dates)
head(source_sub)

range(dates$Intro.Date, na.rm=T)

combo <- left_join(source_sub %>% select(USDA.PLANTS.CODE, forsale),
                   dates %>% select(USDA.PLANTS.CODE, Intro.Date),
                   by="USDA.PLANTS.CODE")
head(combo)
range(combo$Intro.Date, na.rm=T)
str(combo)

tapply(combo$Intro.Date, combo$forsale, FUN=range, na.rm=T)
print(combo %>% filter(Intro.Date==1985))
print(source_sub %>% filter(USDA.PLANTS.CODE=="CILI5"))

hist(combo$Intro.Date)

# What are the intro pathways that aren't ornamental?
source$plant_trade <- ifelse(source$USDA.PLANTS.CODE %in% source_sub$USDA.PLANTS.CODE, "Y", "N")
table(source$plant_trade)

head(source)
table(source$Intentional, source$plant_trade)

print(source %>% filter(plant_trade=="N" & Intentional==1 & searched=="Y") %>% 
        select(Intentional.Introduction.Type) %>% 
        distinct() %>% droplevels())

print(source %>% filter(plant_trade=="N" & Intentional==0 & searched=="Y") %>% 
        select(Accidental.Introduction.Type) %>% 
        distinct() %>% droplevels())

