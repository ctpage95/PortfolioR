

# eSports
#install.packages("httr")
#install.packages("jsonlite")


library(httr)
library(jsonlite)

get1 <- GET("https://s3-us-west-1.amazonaws.com/riot-developer-portal/seed-data/matches10.json")
alljson1 <- fromJSON(content(get1, "text", encoding = "UTF-8"))

# Gather the data for all 100 participants
big_df <- NULL

for (l in 1:100) {
  this.row.i <- data.frame(
    Win = ifelse(alljson1$matches$participants[[l]]$stats$win==TRUE, 1, 0),
    Kills = alljson1$matches$participants[[l]]$stats$kills,
    Deaths = alljson1$matches$participants[[l]]$stats$deaths,
    Assists = alljson1$matches$participants[[l]]$stats$assists,
    GoldEarned = alljson1$matches$participants[[l]]$stats$goldEarned,
    LongestTimeSpentLiving = alljson1$matches$participants[[l]]$stats$longestTimeSpentLiving,
    LargestMultiKill = alljson1$matches$participants[[l]]$stats$largestMultiKill
    )
  big_df <- rbind(big_df,this.row.i)
}

par(mfrow = c(2,3))
boxplot(Kills ~ Win, data = big_df, 
        main = "Kills and Wins", xlab = "Win", ylab = "Kills")
boxplot(Deaths ~ Win, data = big_df, 
        main = "Deaths and Wins", xlab = "Win", ylab = "Deaths")
boxplot(Assists ~ Win, data = big_df, 
        main = "Assists and Wins", xlab = "Win", ylab = "Assists")
boxplot(GoldEarned ~ Win, data = big_df, 
        main = "GoldEarned and Wins", xlab = "Win", ylab = "Gold Earned")
boxplot(LongestTimeSpentLiving ~ Win, data = big_df, 
        main = "Time Living and Wins", xlab = "Win", ylab = "Longest Time Spent Living")
boxplot(LargestMultiKill ~ Win, data = big_df, 
        main = "Multi Kill and Wins", xlab = "Win", ylab = "Largest Multi Kill")
par(mfrow = c(1,1))

# Remove obs 175, 322, 374, 526, 792, and any obs with kills >= 20 & Deaths >= 15
big_df <- big_df[-c(175, 322, 374, 526, 792),]
big_df <- subset(big_df, Kills < 20 & Deaths < 15)
dim(big_df)

# Response Variable: Win = 1 if won, Win = 0 if lost
# Explanatory Variables
#   Offense: Kills, GoldEarned
#   Errors (Things you don't want): Death
#   Team Play: Assists
#   Risk/Reward: LongestTimeSpentLiving
#   Hot Hand: LargestMultiKill

# Create a training and test data set
set.seed(58)
train_ind <- sample(989, 700)

big_df_train <- big_df[train_ind,]
big_df_test <- big_df[-train_ind,]

mean(big_df_test$LongestTimeSpentLiving)
mean(big_df_train$LongestTimeSpentLiving)

# Analysis:
# Since the outcome is either a zero or a one, it is a logistic model
# Model: log( P(Win) / P(Losing) ) = beta0 + beta1 Kills + beta2 GoldEarned + beta3 Deaths
#                           + beta4 Assists + beta5 LongestTimeSpentLiving + beta6 LargestMultiKill

# No epsilon because we are modeling the probability and not the data

big_df_out <- glm(Win ~ Kills + GoldEarned + Deaths+ Assists + LongestTimeSpentLiving + LargestMultiKill,
                 data = big_df_train,
                 family = "binomial")

summary(big_df_out)


# Interpret effects:

# Change in odds interpretation
exp(coef(big_df_out)[-1])

# 95% CI on log-odds
confint(big_df_out)

# 95% CI on change in odds
exp(confint(big_df_out)[-1,])



# Graphics of effects
# Kills:

par(mfrow = c(2,2))
# In terms of log-odds
x_star <- seq(0, 20, length = 100)
plot(x_star, coef(big_df_out)[2]*x_star, type = "l",
     ylim = c(-10,1.2),
     xlab = "Kills", ylab = "Partial logit(Win)")  #Partial Log-Odds of Winning")

# Gold Earned
x_star3 <- seq(0,3000, length = 100)
plot(x_star3, coef(big_df_out)[3] * x_star3, type = "l",
     ylim = c(-10,1.2),
     xlab = "Gold Earned", ylab = "Partial logit(Death)")

# Deaths
x_star2 <- seq(0,15, length = 100)
plot(x_star2, coef(big_df_out)[4] * x_star2, type = "l",
     ylim = c(-10,1.2),
     xlab = "Deaths", ylab = "Partial logit(Death)")

# Assists
x_star4 <- seq(0,15, length = 100)
plot(x_star4, coef(big_df_out)[5] * x_star4, type = "l",
     ylim = c(-10,1.2),
     xlab = "Assists", ylab = "Partial logit(Death)")

par(mfrow = c(1,1))


# From a Probability Perspective (set all other explanatory variables to median)
#   Demonstrate effect of Kills
x_star <- data.frame(Kills = seq(0,10,length = 100),
                     GoldEarned = 11000, Deaths = 5, Assists = 7,
                     LongestTimeSpentLiving = 600, LargestMultiKill = 1)

plot(x_star$Kills, predict(big_df_out, newdata = x_star, type = "response"),
     type = "l", xlab = "Kills", ylab = "Win Probability", ylim = c(0,1))

# Demonstrate effect for GoldEarned
x_star2 <- data.frame(GoldEarned = seq(5000,17500,length = 100),
                     Kills = 5, Deaths = 5, Assists = 7,
                     LongestTimeSpentLiving = 600, LargestMultiKill = 1)

plot(x_star2$GoldEarned, predict(big_df_out, newdata = x_star2, type = "response"),
     type = "l", xlab = "Gold Earned", ylab = "Win Probability", ylim = c(0,1))


# Summarize the statistical significance of the model factors
summary(big_df_out)
# Or use 95% confidence intervals

# Does being aggressive have a statistically significant effect
# Ho: No effect on winning for "aggressive" strategy
# H0: No Kills or LargestMultiKill or GoldEarned effect

# Assuming null hypothesis is true, we delete the factors we are observing
LOL_reduced <- glm(Win ~ Deaths+ Assists + LongestTimeSpentLiving,
                 data = big_df_train,
                 family = "binomial")
# Compare
anova(LOL_reduced, big_df_out, test = "Chisq")

# Predict the probability of winning for a player with Faker-like skills
predict(big_df_out, newdata = data.frame(Kills = 2, GoldEarned = 15000,
                                         Deaths = 2, Assists = 8, 
                                         LongestTimeSpentLiving = 600,
                                         LargestMultiKill = 2
                                         ), 
        type = "response")

# 95% Confidence Interval on the probability of winning
Faker_logit <- predict(big_df_out, newdata = data.frame(Kills = 2, GoldEarned = 15000,
                                         Deaths = 2, Assists = 8, 
                                         LongestTimeSpentLiving = 600,
                                         LargestMultiKill = 2
                                         ), 
        type = "link", se.fit = TRUE)

# Assemble the 95% confidence interval of logit(Win)
logit_L <- Faker_logit$fit - 1.96*Faker_logit$se.fit
logit_U <- Faker_logit$fit + 1.96*Faker_logit$se.fit

# Transform Logit to probability
Faker_phat_L <- exp(logit_L) / (1 + exp(logit_L))
Faker_phat_U <- exp(logit_U) / (1 + exp(logit_U))

# Construct the ROC curve

# It is important because it helps us decide where to choose the cutoff between 
# classifying game as win or loss. We can use this curve where to choose the 
# point where we get max ... between the two. Where am I willing to deal with 
# consequences of error. 

# What we're looking for - a reference line, we want to see a line that hugs
#   the extremes
library(ROCR)
train_pred <- prediction(predict(big_df_out, type = "response"), big_df_train$Win)
train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr")
plot(train_perf, xlab = "1-specificity", ylab = "Sensitivity", main = "ROC")
abline(0, 1, col = "gray")

test_pred <- prediction(predict(big_df_out, newdata = big_df_test, 
                                type = "response"),
                                big_df_test$Win)
test_perf <- performance(test_pred, measure = "tpr", x.measure = "fpr")
plot(test_perf, add = TRUE, col = "blue")

# AUC
performance(train_pred, measure = "auc")
performance(test_pred, measure = "auc")





