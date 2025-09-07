# 0.) Dependencies ----
# Based on: http://www3.biotech.wisc.edu/brc/workshop/SROP/tabr/index.html

library(haven)
library(tidyverse)
library(nhanesA)
library(tidyverse)
library(tidytext)
library(survey)

# 1.) Read in data ----
# read in 2021-2023 demography data
demog_data <- nhanes('DEMO_L')

# rename columns to be human readable
demog_data_1 <- demog_data %>% 
  rename(fpl = INDFMPIR, #Ratio of family income to poverty	(0-5)
         age = RIDAGEYR, #Age in years at screening	(0-80)
         gender = RIAGENDR, #Gender (1-2)
         interviewWeights = WTINT2YR, #Full sample 2 year interview weight (3293.928267 - 233755.84185)
         examWeights = WTMEC2YR, #Full sample 2-year MEC exam weight
         psu = SDMVPSU, #Masked variance pseudo-PSU	(1 to 2)
         strata = SDMVSTRA) #Masked variance pseudo-stratum	(119 to 133)

# 2.) Data Cleaning ----
nhanesAnalysis <- demog_data_1 %>% 
  # select columns
  select(fpl, age, gender, persWeights, psu, strata) %>% 
  # change gender to binary factor
  mutate(gender = as.factor(gender))

# take a peek
head(nhanesAnalysis)  
dim(nhanesAnalysis)

# 3.) Add survey weights to this data ----
# Here we use "svydesign" to assign the weights. We will use this new design
# variable "nhanesDesign" when running our analyses.
# sampling information: https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X200800210759 
nhanesDesign <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~persWeights,
  nest = TRUE,
  data = nhanesAnalysis
)

# Use subset to tell the survey object to limit population to those between 18 and 79 years old
ageDesign <- subset(nhanesDesign, age > 17 &
                      age < 80)

# use svymean to calculate mean and SE of the age, excluding NAs
svymean(~age, ageDesign, na.rm = TRUE)

# svymean does proportions for categorical vars like gender
svymean(~gender, ageDesign, na.rm = TRUE)
# there are more women than men in this subpopulation

# family income to poverty ratio
svymean(~fpl, ageDesign, na.rm = TRUE)
mean((nhanesAnalysis %>% filter(age > 17 & age < 80))$fpl, na.rm = TRUE)
# 3.05 is the mean ratio of family income to poverty, suggesting a slightly more financially prosperous population
# can see a real difference when not using sample weights as well as the stand alone mean is about 2.89

# 4.) Analysis ----
# some modelling information: https://www.bookdown.org/rwnahhas/RMPH/ 

# visualize our variables
svyhist(~age, ageDesign)
barplot(svytable(~gender, ageDesign))
svyhist(~fpl, ageDesign)

# run a general linear model with gaussian link function to assess ratio of family income to poverty by age and gender
model1_output <- svyglm(
  fpl ~ age + gender,
  family = gaussian(), # the standard link to use when we have no evidence of non normality in outcome given the predictors
  design = ageDesign
  )
model1_output
# age is positively associated with fpl (0.01292)
# being female rather than male is negatively associated with fpl (-0.24833)

