#install.packages("nhanesA")

library(nhanesA)
library(tidyverse)
library(tidytext)

# Download dietary data and demography data
# https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Dietary&Cycle=2021-2023

# Dietary Interview - Total Nutrient Intakes, First Day
diet_data <- nhanes('DR1TOT_L')
demog_data <- nhanes('DEMO_L')

# 1.) Combine demographic data and Total Nutrient Intakes, First Day data
fd1_comb <- diet_data %>% 
  left_join(demog_data, by = 'SEQN')

# some stats about respondents
fd1_comb_refined <- fd1_comb %>%
  # ID, education, # in household, marital status, age, gender,  
  select(SEQN, DMDEDUC2, DMDHHSIZ, DMDMARTZ, RIDAGEYR, RIAGENDR, RIDRETH1, RIDRETH3) %>% 
  mutate(across(everything(), ~as.character(.)))

# pivot for aggregation
fd1_comb_sum <- fd1_comb_refined %>% 
  pivot_longer(cols = -SEQN, names_to = 'variable', values_to = 'value') %>% 
  summarise(respondents = n_distinct(SEQN), .by = c(variable, value)) %>% 
  left_join(
    fd1_comb_refined %>% 
      pivot_longer(cols = -SEQN, names_to = 'variable', values_to = 'value') %>% 
      summarise(total_respondents = n_distinct(SEQN), .by = c(variable)),
    by = 'variable'
  ) %>% 
  mutate(pct = respondents / total_respondents,
         pct_label = scales::percent(pct, 0.1))
fd1_comb_sum

# visualize
fd1_comb_plot <- fd1_comb_sum %>% 
  filter(variable != 'RIDAGEYR') %>% 
  ggplot(aes(x = pct, y = reorder_within(value, pct, variable), fill = variable))+
  geom_col(width = 0.8) +
  scale_y_reordered()+
  scale_x_continuous(expand = expansion(mult = c(0,0.25)))+
  facet_wrap(.~variable, ncol = 2, scales = 'free_y')+
  theme(legend.position = 'none') +
  geom_text(aes(label = pct_label, hjust = -0.1))+
  labs(y = '', x = 'Percent of Respondents')
fd1_comb_plot

# 2.) Analyze diet by education level
# fd1_comb_sum <- fd1_comb %>%
#   select(SEQN, DR1TPROT)
#   
#   pivot_longer()
#   summarise(mean_protein = mean(DR1TPROT, na.rm = TRUE), .by = DMDEDUC2) %>% 
#   mutate(category = 'Education Level') %>% 
#   arrange(desc(mean_protein))
# fd1_comb_sum
