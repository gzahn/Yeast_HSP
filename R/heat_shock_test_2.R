library(tidyverse)
library(readxl)
library(lubridate)
library(growthcurver)
theme_set(theme_minimal())

#  bottom row is control. (H)
#  Top 3 rows were shocked at 54 c for 3 minutes. (A,B,C) 
#  Next 4 rows were just put in after the shock. (D,E,F,G)
#  WT (cols 1:4) - SSA1 (cols 5:8) - SSA2 (cols 9:12)


# double-check results range on sheet
sheetrange <- "B38:CU79"
timepoints <- 0:40


# read data
  df <- read_xlsx("./data/pilot_data/Top_3_54_c_3_minutes_40_Hours_take2.xlsx",range = sheetrange)

# convert time format to time points
df$Time <- timepoints

# find control absorbance
controls <- df %>% 
  select(starts_with("H")) %>% 
  pivot_longer(everything())

# find genotypes by location
WTcols <- paste0(LETTERS[1:7],rep(1:4,each=7))
SSA1cols <- paste0(LETTERS[1:7],rep(5:8,each=7))
SSA2cols <- paste0(LETTERS[1:7],rep(9:12,each=7))

# tidy data - add genotype and treatment info
df_clean <- df %>% 
  select(-`T° 600`) %>% 
  select(-starts_with("H")) %>% 
  pivot_longer(-Time) %>% 
  mutate(absorbance = value - mean(controls$value)) %>% 
  mutate(absorbance = case_when(absorbance < 0 ~ 0,
                                TRUE ~ absorbance)) %>% 
  rename(time=Time,well=name) %>% 
  select(-value) %>% 
  mutate(treatment = case_when(grepl("^A|^B|^C",well) ~ "54 deg shock",
                               TRUE ~ "No shock"),
         genotype = case_when(well %in% WTcols ~ "WT",
                              well %in% SSA1cols ~ "SSA1",
                              well %in% SSA2cols ~ "SSA2")) %>% 
  mutate(genotype = factor(genotype,levels = c("WT","SSA1","SSA2"))) %>% 
  mutate(row = str_sub(well,1,1),
         col = str_sub(well,2,3) %>% factor(levels = 1:12))


# plot all wells
df_clean %>% 
  filter(time <= 24) %>% 
  ggplot(aes(x=time,y=absorbance,color=genotype,shape=treatment,linetype=treatment)) +
  geom_smooth() +
  facet_grid(rows = vars(row),
             cols = vars(col)) +
  theme(axis.text.x = element_blank())




# plot absorbance over time
df_clean %>% 
  filter(time <=24) %>% 
  ggplot(aes(x=time,y=absorbance,color=treatment)) +
  geom_smooth() +
  facet_wrap(~genotype)
ggsave("./output/figs/54deg_shock_24hr_comparison_allgenotypes.png")

# model "endpoint"
df_clean %>% 
  filter(time==24) %>% 
  glm(data=.,
      formula = absorbance ~ treatment * genotype) %>% 
  summary()




#### Below this is messing with the logistic fits. Gotta be an easier way to process the whole plate at once for these curves.


# prep for growthcurver package
summary_abs <- df_clean %>% 
  group_by(time,treatment,genotype) %>% 
  summarize(mean_abs = mean(absorbance))

# WT control
WTC <- summary_abs %>% 
  filter(genotype=="WT" & treatment == "54 deg shock")
WTC$time
gc <- growthcurver::SummarizeGrowth(WTC$time,WTC$mean_abs)
gc$vals$k

