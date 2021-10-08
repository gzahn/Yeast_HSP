# packages and options ####
library(tidyverse)
library(janitor)
library(readxl)
library(report)
library(lubridate)
theme_set(theme_classic())


# Gather up data and plate metadata ####

# name of file
filename <- "./data/practice/Yeast2_EveryHour_10_Run1.xlsx"


# header metadata about plate conditions
plate_metadata <- read_xlsx(filename,range="A2:B19")

# combine excel date and time values / convert to POSIXct
dt <- plate_metadata[5,2] %>% as.numeric() + plate_metadata[6,2] %>% as.numeric()
dt <- convert_to_datetime(dt,tz = "MDT")

# read conditions
conditions <- plate_metadata[17,2] %>% c() %>% as.character()

# get well sample names
samplenames <- read_xlsx(filename,range="B26:N34")

# read in plate absorbance data
df <- read_xlsx(filename,range="B38:CU49")
df$Time <- df$Time %>% lubridate::hour()

# Get negative control values
negative <- data.frame(time=df$Time,
                       abs=df %>% select(starts_with("H")) %>% rowSums() / 12)

# remove negatives from real samples
reads <- df %>% 
  select(!starts_with("H")) %>% 
  select(!starts_with("T"))


# correct from negative controls (per each time period)
corrected <- apply(reads,2,function(x){x-negative$abs}) %>% 
  as.data.frame() %>% 
  mutate(time=df$Time)

# convert to long
long <- corrected %>% 
  pivot_longer(1:(length(names(corrected))-1))


# plot
long %>% 
  ggplot(aes(x=time,y=value)) +
  geom_point() +
  geom_smooth() +
  labs(y="Absorbance at 600nm",x="Time (hrs)")
# 12 hours wasn't long enough to see the S-curve flatten out, but the results are still very encouraging
# very odd that there are two distinct groupings. Were any samples treated with a stressor?

# heatmap
apply(reads,2,function(x){x-negative$abs}) %>% 
  t() %>% 
  heatmap(Rowv = NA,Colv = NA)
# looks like some consistent pipette error (first row is more variable than the rest...could also be evaporation?)



# add row and col info as columns to troubleshoot
long <- long %>% 
  mutate(ROW=str_sub(name,start = 1,end=1)) %>% 
  mutate(COL=str_sub(name,start = 2,end=3)) 
# plot again with row facets
long %>% 
  ggplot(aes(x=time,y=value,color=ROW)) +
  geom_point() +
  facet_wrap(~factor(ROW))
# plot again with column facets
long %>% 
  ggplot(aes(x=time,y=value,color=COL)) +
  geom_point() +
  facet_wrap(~factor(COL))

# facet by well, just for shits
long %>% 
  ggplot(aes(x=time,y=value)) +
  geom_smooth(se=FALSE) +
  facet_grid(rows = vars(factor(ROW)),cols=vars(as.numeric(COL)))
ggsave("./data/practice/growth_cruve_fullplot.png",height = 7,width = 12)

