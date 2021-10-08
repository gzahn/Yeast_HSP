# packages and options ####
library(tidyverse)
library(janitor)
library(readxl)
library(report)
theme_set(theme_classic())


# Gather up data and plate metadata ####

# names of files
path <- "./data/practice"
filenames <- list.files(path, full.names = TRUE)


df_list <- list()

# read in all files in the given path, process and save each as list element ####
for(i in 1:length(filenames)){
  
# header metadata about plate conditions
plate_metadata <- read_xlsx(filenames[i],range="A2:B19")
# combine excel date and time values / convert to POSIXct
dt <- plate_metadata[5,2] %>% as.numeric() + plate_metadata[6,2] %>% as.numeric()
dt <- convert_to_datetime(dt,tz = "MDT")
# wavelength
wavelength <- plate_metadata[16,2] %>% c() %>% as.character()
# read conditions
conditions <- plate_metadata[17,2] %>% c() %>% as.character()
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
df$Timepoint = dt %>% as.POSIXct()


df_list[[i]] <- df

}

# combine all generated data frames into one ####
full <- df_list %>% 
  reduce(full_join)


# plot curve ####
full %>% 
  ggplot(aes(x=Timepoint,y=OD,color=Treatment,group=Treatment)) +
  geom_point() +
  geom_smooth() +
  labs(title="Growth curve")
