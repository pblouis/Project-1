library(tidyverse)
#Separating data into treatment and control groups
Treatment <- filter(cancer_in_dogs, order == "2,4-D")
Control <- filter(cancer_in_dogs, order == "no 2,4-D")
#Sum up number of dogs with cancer
Exposed <- sum(Treatment$response == "cancer")
Not_Exposed <- sum(Control$response == "cancer")
#Performing right tailed binomial test
binom.test(Exposed, 491, Not_Exposed/945, alternative="greater")

#Section 2
#Removing rows with missing data
GGI_omitted <- na.omit(GGI2013)
#Removing first row with the years
GGI_data <- filter(GGI_omitted, V3 != 2013.0000, V8 != 2008.0000)
#Performing paired t-test
t.test(GGI_data$V8, GGI_data$V3, paired = TRUE, alternative = "two.sided")

#Plotting Section
Lyme_Disease_Cases <- read.csv("~/Downloads/Lyme_Disease_Cases_by_Year_and_Region_United_States.csv")
#Loading ggplot
library(ggplot2)
#Creating double bar graph
ggplot(Lyme_Disease_Cases, aes(Year, Cases, fill = Region)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(title="Lyme Disease Cases by Year and Region in the U.S.")

