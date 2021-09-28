#load packages
library(dplyr)
library(ggplot2)
library(statsr)

#simulating probability in R
#Getting a general view of kobe's basketball data
#Calculate the shot streak of kobe
kobe_streak <- calc_streak(kobe_basket$shot)

#plot histogram of the acquired data
ggplot(data = kobe_streak, aes(x=length)) + geom_histogram(binwidth=1)

#Assuming the coin is not rigged and having a big sample size the percentage
#distribution between heads and tails should be a 50:50 split. This can be 
#simulated with the following code.
coin_outcomes <- c('head', 'tails')
sample(coin_outcomes,size=1, replace = TRUE)
sim_fair_coin <- sample(coin_outcomes, size=100, replace = TRUE)

#We can also rig the coin to see an 80:20(heads, tails) split or vice versa
sim_fair_coin <- sample(coin_outcomes, size=100, replace = TRUE, 
                        prob = c(0.2,0.8))

#View results of simulartion through a table
table(sim_fair_coin)

#Check for independence in Kobe's shots
shot_outcomes <- c('H','M')
sim_basket <- sample(shot_outcomes, size = 133, replace = TRUE)

#alter sim_basket so that it reflects Kobe's shot percentage
sim_basket <- sample(shot_outcomes, size = 133, replace = TRUE,
                     prob = c(0.45, 0.55))

#calculate shot streak of sim_basket
sim_streak <- calc_streak(sim_basket)
table(sim_streak)

#create a histogram of sim_streak
ggplot(data=sim_streak, aes(x=length)) + geom_histogram(binwidth=1)

#compare kobe's distritbution to a simulated distritbution
player_streak <- c('H', 'M')
sim_player_streak <- sample(player_streak, size=133, replace=TRUE,
                            prob = c(0.5,0.5))
sim_player_calc_streak <- calc_streak(sim_player_streak)
table(sim_player_calc_streak)

#create histogram
ggplot(data=sim_player_calc_streak, aes(x=length)) + geom_histogram(binwidth = 1)

#both kobe and simulated player have a similar distribution, showing that
#streaks are independent from each other. 