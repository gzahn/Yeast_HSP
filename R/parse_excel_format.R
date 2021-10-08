# packages and options ####
library(tidyverse)
library(janitor)
library(readxl)
library(report)
theme_set(theme_classic())


# Gather up data and plate metadata ####

# name of file
filename <- "./data/practice/Yeast1-timepoint0.xlsx"


# header metadata about plate conditions
plate_metadata <- read_xlsx(filename,range="A2:B19")

# combine excel date and time values / convert to POSIXct
dt <- plate_metadata[5,2] %>% as.numeric() + plate_metadata[6,2] %>% as.numeric()
dt <- convert_to_datetime(dt,tz = "MDT")

# wavelength
wavelength <- plate_metadata[16,2] %>% c() %>% as.character()

# read conditions
conditions <- plate_metadata[17,2] %>% c() %>% as.character()


# read in plate absorbence data
df <- read_xlsx(filename,range="C36:N44")

# Clean Data ####

# convert to long format
STD <- df[1:7,1:6] %>% c() %>% unlist()
STD_control <- df[8,1:6] %>% unlist() %>% mean()

SALT <- df[1:7,7:12] %>% c() %>% unlist()
SALT_control <- df[8,7:12] %>% unlist() %>% mean()

# normalize values against mean of respective controls
STD <- STD - STD_control
SALT <- SALT - SALT_control

# remove any negative values that may now exist / convert to 0
STD[STD<0] <- 0
SALT[SALT<0] <- 0

# Build dataframe
df <- data.frame(Control = STD,
           Stressed = SALT) %>% 
  pivot_longer(1:2,names_to = "Treatment",values_to = "OD")


# quick plot and test ####
df %>% 
  ggplot(aes(x=Treatment,y=OD,fill=Treatment)) +
  geom_boxplot() +
  labs(title = as.character(dt),
       subtitle ="Sodium acetate (3M)",
       y=paste0("OD (",wavelength,")"),
       caption = conditions)

mod <- aov(data=df,
           formula = OD ~ Treatment)

summary(mod)
TukeyHSD(mod)
report(mod)
