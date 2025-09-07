#install.packages("nhanesA")
library(nhanesA)
library(tidyverse)
library(tidytext)
library(haven)
library(survey)
library(scales)

# Download dietary data and demography data
# https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Dietary&Cycle=2021-2023

# Dietary Interview - Total Nutrient Intakes, First Day
diet_data <- nhanes('DR1TOT_L')
demog_data <- nhanes('DEMO_L')

# 0.) Clean and rename vars and incorporate survey weights ----
demog_data_clean <- demog_data %>% 
  rename(fpl = INDFMPIR, #Ratio of family income to poverty	(0-5)
         age = RIDAGEYR, #Age in years at screening	(0-80)
         gender = RIAGENDR, #Gender (1-2)
         interviewWeights = WTINT2YR, #Full sample 2 year interview weight (3293.928267 - 233755.84185)
         examWeights = WTMEC2YR, #Full sample 2-year MEC exam weight
         psu = SDMVPSU, #Masked variance pseudo-PSU	(1 to 2)
         strata = SDMVSTRA,
         RaceHispanicOrigin	 = RIDRETH1, 
         RaceHispanicOriginNHAsian = RIDRETH3,
         highestEducation = DMDEDUC2,
         maritalStatus = DMDMARTZ) %>% 
  # Change gender to binary factor
  mutate(gender = as.factor(gender)) %>% 
  select(SEQN, gender, age, fpl, interviewWeights, examWeights, psu, strata, highestEducation, maritalStatus, RaceHispanicOriginNHAsian)

# look at data
head(demog_data_clean)

# specify survey design here (using interview weights as we are not looking at exam data yet)
nhanesDesign <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~interviewWeights,
  nest = TRUE,
  data = demog_data_clean
)

# make a further survey design specifying ages 18 to 80 that we will look at
ageDesign <- subset(nhanesDesign, age > 17 &
                      age <= 80)

# 1.) Create a weighted table ----

# gender by education level
weighted_gender_edu_tbl <- svytable(~gender + highestEducation, ageDesign)
weighted_gender_edu_tbl

# visualize by age
weighted_gender_edu_df <- as.data.frame(weighted_gender_edu_tbl)

# raw totals
weighted_gender_edu_df %>% 
  ggplot(aes(x = Freq, y = reorder(highestEducation, +Freq), fill = gender))+
  geom_col(position = position_dodge2(width = 0.8)) +
  scale_fill_manual(values = c('Male' = 'blue', 'Female' = 'lightblue'))+
  scale_x_continuous(expand = expansion(mult = c(0,0.45)), labels = comma_format())+
  geom_text(aes(label = scales::comma(Freq, 1), hjust = -0.1), position = position_dodge2(width = 0.8))
  
# gender percents by education level
weighted_gender_edu_df2 <- weighted_gender_edu_df %>% 
  mutate(total_freq = sum(Freq), .by = c(highestEducation)) %>% 
  mutate(pct = Freq / total_freq,
         pct_label = percent(pct, 1))

weighted_gender_edu_df2 %>% 
  ggplot(aes(x = pct, y = reorder(highestEducation, +Freq), fill = gender))+
  geom_col(position = position_dodge2(width = 0.8)) +
  scale_fill_manual(values = c('Male' = 'blue', 'Female' = 'lightblue'))+
  scale_x_continuous(expand = expansion(mult = c(0,0.45)), labels = comma_format())+
  geom_text(aes(label = pct_label, hjust = -0.1), position = position_dodge2(width = 0.8))

# percent of all respondents
weighted_gender_edu_df3 <- weighted_gender_edu_df %>% 
  mutate(total_freq = sum(Freq)) %>% 
  mutate(pct = Freq / total_freq,
         pct_label = percent(pct, accuracy = .1))

weighted_gender_edu_df3 %>% 
  ggplot(aes(x = pct, y = reorder(highestEducation, +Freq), fill = gender))+
  geom_col(position = position_dodge2(width = 0.8)) +
  scale_fill_manual(values = c('Male' = 'blue', 'Female' = 'lightblue'))+
  scale_x_continuous(expand = expansion(mult = c(0,0.45)), labels = comma_format())+
  geom_text(aes(label = pct_label, hjust = -0.1), position = position_dodge2(width = 0.8))

# 2.) Combine demographic data and Total Nutrient Intakes, First Day data ----
fd1_drd1 <- diet_data %>% 
  left_join(demog_data_clean, by = 'SEQN')

# clean data
fd1_drd1_clean <- fd1_drd1 %>%
  # rename initial dietary intake data:
  rename(
    kcal = DR1TKCAL,
    protein = DR1TPROT,
    carbs = DR1TCARB,
    sugar = DR1TSUGR,
    totalFat = DR1TTFAT,
    saturatedFat = DR1TSFAT,
    firstDayWeights = WTDRD1
  ) %>% 
  # choose columns
  select(
    SEQN, 
    gender, 
    age, 
    fpl, 
    interviewWeights, 
    examWeights,
    firstDayWeights,
    psu, 
    strata, 
    highestEducation, 
    maritalStatus, 
    RaceHispanicOriginNHAsian,
    kcal, 
    protein,
    carbs,
    sugar,
    totalFat,
    saturatedFat
  ) #%>% 
  #mutate(across(everything(), ~as.character(.)))

# specify survey design here (using interview weights as we are not looking at exam data yet)
nhanesDesignD1 <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~ firstDayWeights,
  nest = TRUE,
  data = fd1_drd1_clean
)

# make a further survey design specifying ages 18 to 80 that we will look at
ageDesignD1 <- subset(nhanesDesignD1, age > 17 &
                      age <= 80)

# calories by gender
weighted_gender_kcal_tbl <- svytable(~gender + kcal, ageDesignD1)
weighted_gender_kcal_tbl

# means by gender
svymean(gender ~ age + kcal, ageDesignD1, na.rm = T)

# pivot for aggregation
# fd1_comb_sum <- fd1_comb_refined %>% 
#   pivot_longer(cols = -SEQN, names_to = 'variable', values_to = 'value') %>% 
#   summarise(respondents = n_distinct(SEQN), .by = c(variable, value)) %>% 
#   left_join(
#     fd1_comb_refined %>% 
#       pivot_longer(cols = -SEQN, names_to = 'variable', values_to = 'value') %>% 
#       summarise(total_respondents = n_distinct(SEQN), .by = c(variable)),
#     by = 'variable'
#   ) %>% 
#   mutate(pct = respondents / total_respondents,
#          pct_label = scales::percent(pct, 0.1))
# fd1_comb_sum
# 
# # visualize
# fd1_comb_plot <- fd1_comb_sum %>% 
#   filter(variable != 'RIDAGEYR') %>% 
#   ggplot(aes(x = pct, y = reorder_within(value, pct, variable), fill = variable))+
#   geom_col(width = 0.8) +
#   scale_y_reordered()+
#   scale_x_continuous(expand = expansion(mult = c(0,0.25)))+
#   facet_wrap(.~variable, ncol = 2, scales = 'free_y')+
#   theme(legend.position = 'none') +
#   geom_text(aes(label = pct_label, hjust = -0.1))+
#   labs(y = '', x = 'Percent of Respondents')
# fd1_comb_plot

# 2.) Analyze diet by education level
# fd1_comb_sum <- fd1_comb %>%
#   select(SEQN, DR1TPROT)
#   
#   pivot_longer()
#   summarise(mean_protein = mean(DR1TPROT, na.rm = TRUE), .by = DMDEDUC2) %>% 
#   mutate(category = 'Education Level') %>% 
#   arrange(desc(mean_protein))
# fd1_comb_sum
