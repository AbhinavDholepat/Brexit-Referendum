install.packages(c("dslabs", "ggplot2", "dplyr", "tidyverse")) 
library(dslabs)
library(ggplot2) 
library(dplyr)
library(tidyverse)

data("brexit_polls")
view(brexit_polls) 

save(brexit_polls, file = "Data/brexit_polls_data.rda") 
