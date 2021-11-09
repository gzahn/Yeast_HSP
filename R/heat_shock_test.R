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
sheetrange <- "B27:CU52"

# read data
df <- read_xlsx("./data/pilot_data/54C_9_Min_top_3_rows_24_Hours.xlsx",range = sheetrange)

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
  mutate(Time=hour(df$Time)) %>% 
  select(-`T° 600`) %>% 
  select(-starts_with("H")) %>% 
  pivot_longer(-Time) %>% 
  mutate(absorbance = value - mean(controls$value)) %>% 
  rename(time=Time,well=name) %>% 
  select(-value) %>% 
  mutate(treatment = case_when(grepl("^A|^B|^C",well) ~ "54 deg shock",
                               TRUE ~ "No shock"),
         genotype = case_when(well %in% WTcols ~ "WT",
                              well %in% SSA1cols ~ "SSA1",
                              well %in% SSA2cols ~ "SSA2")) %>% 
  mutate(genotype = factor(genotype,levels = c("WT","SSA1","SSA2")))


# plot absorbance over time
df_clean %>% 
  ggplot(aes(x=time,y=absorbance,color=genotype)) +
  geom_smooth() +
  facet_wrap(~treatment)
ggsave("./output/figs/54deg_shock_24hr_comparison_allgenotypes.png")

# model "endpoint"
df_clean %>% 
  filter(time==max(time)) %>% 
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

