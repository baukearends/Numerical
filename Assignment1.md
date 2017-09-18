##### Numerical Assignment 1 #####

rm(list=ls())
setwd("/Users/baukearends/Documents/Studie/Minor Econometrie/Numerical Methods for EOR/Assignments")
library("readxl")
library("xtable")
library("plyr")
library("dplyr")
library("mltools")

### Question 1 ###

# Read data
data <- read_excel("assignment1-tennis.xlsx", na = "NA")
names(data) <- tolower(names(data))

# Adjust data formats
data$b365a <- as.numeric(data$b365a)
data$b365b <- as.numeric(data$b365b)
data$month <- format(data$date, format = "%Y-%m")

# Games played grouped by month
gamesplayed <- ldply(by(data, data$month, function(x){
  count(x, "month")
}))
xtable(gamesplayed[,2:3])

# Mean payout
meanA <- mean(data$b365a, na.rm = T)
meanB <- mean(data$b365b, na.rm = T)

ttest <- t.test(data$b365a, data$b365b)



### Question 2 ###

# Calculate implied probabilities
data <- mutate(data, probA = (1/b365a)/((1/b365a)+(1/b365b)), probB = (1/b365b)/((1/b365b)+(1/b365a)),
               winA = asets > bsets, winB = bsets > asets)

probabilities <- as.matrix(rbind(cbind(data$probA, data$winA), cbind(data$probB, data$winB)))
names(probabilities) <- c("implied_win_prob", "win")

# Assign buckets
values = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
values2 <- quantile(probabilities[,1], values, na.rm = T)
values2[1] <- 0; values2[11] <- 1

probabilities <- as.data.frame(probabilities)
probabilities$bucket <- cut(probabilities$V1, values2, labels = F)
names(probabilities) <- c("implied_win_probability", "win", "bucket")
table(probabilities$bucket)

# Calculate implied and actual win percentages
x <- aggregate.data.frame(probabilities$implied_win_probability, by = list(probabilities[,"bucket"]), FUN = function(x){
  mean(x, na.rm = T)
})
y <- aggregate.data.frame(probabilities$win, by = list(probabilities[,"bucket"]), FUN = function(x){
  mean(x, na.rm = T)
})

# Plot data
plot(y[,2] ~ x[,2], xlim = c(0,1), ylim = c(0,1), xlab = "Implied win percentage", 
     ylab = "Actual win percentage", main = "Actual versus implied win percentage")
abline(a = 0, b = 1)
