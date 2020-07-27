### Calculating d value of weighted average S and moe ### 
mean_spread <- final_data %>% 
  summarize(d = (sum(true_spread * samplesize)) / (sum(samplesize))) %>%  
  pull(d)

mean_spread

p_hat <- (mean_spread + 1) / 2 

N <- sum(final_data$samplesize) 

final_se <- 2 * sqrt((p_hat * (1 - p_hat)) / N)

### Confidence Interval of S and moe ### 

mean_spread - (2 * final_se)
mean_spread + (2 * final_se)

### Probability of Voting to leave the EU 
pnorm(0, mean_spread, final_se)

### t test, treating each poll as a part of a basket of polls ### 

avg <- mean(final_data$true_spread)
SSDD <- sd(final_data$true_spread)  
t <- (0 - avg) / (SSDD / sqrt(10)) 
pt(t, 9) 