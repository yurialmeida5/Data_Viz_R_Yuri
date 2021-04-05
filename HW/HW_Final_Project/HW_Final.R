# Load necessary packages

library(data.table)
library(skimr)
library(ggplot2)
library(gganimate)
library(ggiraph)
library(scales)
library(animation)
library(tidyverse)
library(ggrepel)

# Clear Environment 
rm(list = ls())

# Tidytuesday dataset packages
#install.packages("tidytuesdayR")
library(tidytuesdayR)

# Define the graph theme custom function in order to standardize the graph analysis


theme_wealth <- function(){
  
  theme_gray() + 
    theme(plot.caption = element_text(hjust = 0, face = "italic"), 
          plot.title = element_text(hjust = 0.5, color = "blue3", size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          panel.grid.minor = element_blank()) 
  
}


# The Dataset Description 

# The dataset comes from the US Government Bureau Census and highlights some wealth summary statistics by gender and social ethnic group over the past 50 years.
# The data is collected annually through a national survey that includes a probability sample from all the non-institutional American civilians and a few members of the US Armed Forces.
# It's important to mention that wealth is a pretty generic concept and the original raw data is compounded by a different range of separated data tables with divergent specifications like Student Debts, retirement savings, incomes, etc.  
# With that in hands, the idea of this following experiment is to examine those specifications and provide a clear perception of the wealth equality or inequality based on gender or social ethnic group. 

# Load dataset
tuesdata <- tt_load('2021-02-09')
# Reveals the structure 

str(tuesdata)

# As mentioned above, the dataset is made up by a different set of data tables with a vary range of wealth possible indicators
# that include the income distribution, retirement savings, student debts and house ownership by ethnic groups and year.
# Each data table deserve a special treatment their respective analysis and this experiment will evaluate one-by-one.

# Income Distribution 

# In order to reach a good overview regarding the wealth structure within a population, lots of factors should be considered.
# However, undoubtedly, the average income per household is a very important or most important aspect to consider and it's with this factor that the wealth analysis is going to start.
# The income distribution data table contains the mean and median values from 1967 to 2019 of the following ethnic groups:
# Asian, Hispanic, Black and White; still those groups are not well-clearly divided. 
# Some cleaning up stages are necessary to end up with the ones used in this experiment: Hispanic, White and Black.
# Obviously the Asian group could also be considered but it's not present in the other data tables.
# Also, in the cleaning, the years considered were higher than 1971 due to the lack of Hispanic data.

# Cleaning 

income_dist <- data.table(tuesdata$income_distribution)

income_dist <- income_dist[race %in% c('White Alone' , 'Black Alone','Hispanic (Any Race)')][year > 1971]

# Order income_bracket column

income_dist$income_bracket <- factor(income_dist$income_bracket , levels = c('Under $15,000', 
                                               '$15,000 to $24,999',
                                               '$25,000 to $34,999',
                                               '$35,000 to $49,999',
                                               '$50,000 to $74,999',
                                               '$75,000 to $99,999',
                                               '$100,000 to $149,999',
                                               '$150,000 to $199,999',
                                               '$200,000 and over'))

# Rename ethnic group names
income_dist$race <- gsub('(Any Race)', '', income_dist$race)
income_dist$race <- gsub('[[:punct:]]', '', income_dist$race)
income_dist$race <- gsub('Alone', '', income_dist$race)
income_dist$race <- gsub(' ', '', income_dist$race)

income_dist_agg <- income_dist[ , list(income_mean = mean(income_mean),
                                       income_median = mean(income_median),
                                       income_med_moe = mean(income_med_moe),
                                       income_mean_moe = mean(income_mean_moe),
                                       number = mean(number)), by = list(race, year)]

# Income mean along the years by group

IC_GH1 <- ggplot(income_dist_agg, aes(x = year , y = income_mean , color = race)) + 
    geom_line() + geom_point() +
    transition_reveal(year) + 
    labs(title = 'Income Distribuition', 
                 subtitle = 'Total money income of householders separated by year and income groups', 
                   caption = 'Data source: US Census Bureau',
                   y = 'Average Income by Househould (USD)',
                   x = 'Year') +
     scale_x_continuous(limits = c(1972,2019), breaks = seq(1972,2019, by = 5)) +
     scale_y_continuous(breaks = seq(0,105000, by = 15000) , labels = dollar) +
     theme_wealth() +
     theme(legend.position = "top" , legend.title = element_blank())

animate(IC_GH1, end_pause = 10, fps = 5)

# Graph Analysis: Disparities found between the groups (Whites are in Advantage),
# Data follows the same trends during the years, no specific growth or pattern between the groups

# Income distribution by group 

IC_GH2 <- ggplot(income_dist, aes(x = income_bracket, y = income_distribution , fill = race)) +
  geom_bar(stat = 'identity' ) + 
  transition_states(year) +
  labs(title = 'Income Distribuition separated by category and ethnic groups', 
       subtitle = 'Year: {closest_state}', 
       caption = 'Data source: US Census Bureau',
       y = 'Frequency (%)') +
  theme_wealth() +
  theme(legend.position = "none" , axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~race) 
  
animate(IC_GH2, end_pause = 10, fps = 5)

# Graph Analysis: Hispanic and White groups are moving turning their income distribution into a more "normal" shape
# Hispanic: poor class to middle class / White: Middle Class to Upper Class
# Into the black group it's possible to visualize that the inequalities doesn't get much better with time like in the other groups.
# Still a skewed right-tailed distribution with a really few people in the upper class

# Home ownership

house_ownership <- data.table(tuesdata$home_owner)

house_ownership[, home_owner_pct:= round(home_owner_pct * 100,2)]

HO_GH1 <- ggplot(house_ownership, aes(x = year, y = home_owner_pct, color = race)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  theme_wealth() +
  transition_states(race) +
  labs(title = 'Home Ownership', 
       subtitle = 'Ethnic Group: {closest_state}',
       y = 'Percent of Home Ownership by Families (%)') +
  theme(legend.position = "top" , legend.title = element_blank())

animate(HO_GH1, end_pause = 10, fps = 5)

# Graph Analysis: Disparities found between the groups (whites are in clear advantage comparing to other groups),
# Interesting to highlight that along the years Whites and Hispanic People shows a positive trend in terms of house ownerships, 
# However, same aspect can't be seen into the Black community. 

# Student Debt

# Average family student loan debt for aged 25-55, by race and year normalized to 2016 dollars.

student_debt <- data.table(tuesdata$student_debt)

# Average Student Loan Debt per family

SD_GH1 <- ggplot(student_debt, aes(x = year , y = loan_debt , color = race)) + 
  geom_line() + geom_point() +
  transition_reveal(year) + 
  labs(title = 'Student Loan Debt', 
       subtitle = 'Average family student loan debt by ethnic group and year', 
       caption = 'Data source: US Census Bureau',
       y = 'Average Student Loan Debt per Family (USD)',
       x = 'Year') +
  scale_x_continuous(limits = c(1989,2016), breaks = seq(1989,2016, by = 3)) +
  scale_y_continuous(limits = c(0,15000), breaks = seq(0,15000, by = 5000) , labels = dollar) +
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())

# Graph Analysis: Disparities found between the groups (Hispanics are in Advantage),
# Data follows the same trends during the years with a higher growth debt for White and Black people.

#slows down the animation
animate(SD_GH1, end_pause = 10, fps = 5)  

# Share of families with student loan debt

SD_GH2 <- ggplot(student_debt, aes(x = race, y = loan_debt_pct, fill = race)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(x = race, y = loan_debt_pct, label = percent(loan_debt_pct, accuracy = 0.01 )) , vjust = 0) +
  scale_y_continuous(limits = c(0,0.7), breaks = seq(0,1, by = 0.1) , labels = percent) +
  transition_states(year)+
  labs(title = 'Distribuition of the Student Loan Debt by Ethnic Group', 
       subtitle = 'Year: {closest_state}', 
       caption = 'Data source: US Census Bureau',
       y = 'Share of families with student loan debt (%)',
       x = '') +
  theme_wealth() +
  theme(legend.position = "none" , legend.title = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#slows down the animation
animate(SD_GH2, end_pause = 10)  

# Graph Analysis: Disparities found between the groups (Hispanics are in Advantage).
# Follows the same idea as the previous graph. the groups initiate more less the same percentage of the families in debt,
# And there is a big "boom" after 2007 where you see the debt rate increasing substantially for the white and black groups.


# Retirement Savings

retirement  <- data.table(tuesdata$retirement)

RE_GH1 <- ggplot(retirement, aes(x = race, y = retirement, fill = race)) +
  geom_bar(stat = 'identity') +
  theme_wealth() +
  transition_states(year) +
  labs(title = 'Average family liquid retirement savings', 
       subtitle = 'Year: {closest_state}',
       y = 'Average Retirment Savings by Families (USD)') +
  theme(legend.position = "top" , legend.title = element_blank())

animate(RE_GH1, end_pause = 10, fps = 5)

# Graph Analysis: Disparities found between the groups (whites have a huge advantage in terms of the amount of savings)
# However, different from what was observed in the other analysis, that was the first time that it could be observed a greater rate
# increase into the white group compared to the other ones. 

# Final Analysis

joined_dataset <- data.table(income_dist_agg %>% 
  inner_join(house_ownership, by = c('race','year')) %>% 
  inner_join(retirement, by = c('race','year')) %>% 
  inner_join(student_debt, by = c('race', 'year')))

#Remove columns that might be bringing some bias
joined_dataset[, c("income_med_moe", "income_mean_moe" , "number"):= NULL]

  
ani.options(interval = 2)
final_graph <- saveGIF(
    for (i in sort(unique(joined_dataset$year))){
      data_subset <- joined_dataset[year == i]
      rownames(data_subset) <- data_subset$race
      data_subset$race <- NULL
      mds <-as.data.frame(cmdscale(dist(scale(data_subset))))
      mds$race <- rownames(data_subset)
      p <- ggplot(mds, aes(V1, V2, label = race)) +
        labs(title = 'CMD Scaling by Ethnic Group', 
             subtitle = paste0('Year: ', i), 
             caption = 'Data source: US Census Bureau') +
        xlim(-4,4) + ylim(-4,4) +
        geom_text_repel() + theme_wealth()
      print(p)
    }  
  )


# Conclusion 

# Having a quick look at the graphs, it's possible to observe that although the White people has a considerable better initial stage 
# in all categories (income, retirement savings, etc.) there is no clear conclusion about any sort of discrimination regarding the other ethnic groups. 
# This could be explained due to the similar growth rates observed and social indicators seen into the Hispanic group.
# Although the difference in absolute values can be significant, there was a really closer improvement observed into the social indicators between White and Hispanics.



