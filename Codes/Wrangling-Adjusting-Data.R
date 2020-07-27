### Figuring out creadible pollsters ### 

brexit_polls %>% 
  group_by(pollster) %>% 
  summarize(n())


### Filtering Data, one week before election, true spread and true se ### 

data <- brexit_polls %>% 
  mutate(true_remain = (remain / (remain + leave)), true_leave = (leave / (remain + leave))) %>% 
  mutate(true_spread = (true_remain - true_leave)) %>% 
  mutate(true_p = ((true_spread + 1) / 2)) %>% 
  mutate(se = (2 * sqrt(((true_p) * (1 - true_p)) / samplesize))) %>% 
  mutate(true_lower = (true_spread - (1.96 * se)), true_upper = (true_spread + (1.96 * se))) 


final_data <- data %>% 
  group_by(pollster) %>% 
  filter(n() >= 7) %>%
  filter(enddate == max(enddate)) %>%
  ungroup() %>% 
  filter(enddate <= "2016-06-22")

save(final_data, file = "Data/final_data.rda") 


### Graph with true lower and upper ### 

final_data %>% 
  ggplot(aes(enddate)) + 
  geom_line(aes(y = true_lower), color = "Blue") + 
  geom_line(aes(y = true_upper), color = "Red") + 
  xlab("Time Poll Pubished") + 
  ylab("95% Confidence Interval Of Spread") 

ggsave("Visuals/Confidence-Interval-True-Spread.png") 
