### Graph To Show Brexit Polls ###

brexit_polls %>% 
  ggplot(aes(x = enddate)) + 
  geom_line(aes(y = remain), color = "Blue") + 
  geom_line(aes(y = leave), color = "Red") +
  xlab("Time Poll Published") + 
  ylab("Blue is Remain, Red is Leave")

ggsave("Visuals/Remain-Leave-Graph.png")


### Graph Of The Spread ### 

brexit_polls %>% 
  ggplot(aes(enddate, spread)) + 
  geom_line(color = "Orange") + 
  xlab("Time Poll Published") + 
  ylab("Spread") 

ggsave("Visuals/Spread-Graph.png") 



### Creating a 95% CI for the poll spreads ### 


brexit_polls <- brexit_polls %>% 
  mutate(p = ((spread + 1) / 2)) %>% 
  mutate(se = sqrt((p * (1 - p)) / samplesize)) %>% 
  mutate(lower = (spread - (2 * se)), upper = (spread + (2 * se))) 

### Graphing The 95% CI ### 

brexit_polls %>% 
  ggplot(aes(x = enddate)) + 
  geom_line(aes(y = lower), color = "Red") + 
  geom_line(aes(y = upper), color = "Blue") +
  xlab("Time Poll Published") + 
  ylab("95% Confidence Interval of Spread") 

ggsave("Visuals/Confidence-Interval-Spread.png") 
