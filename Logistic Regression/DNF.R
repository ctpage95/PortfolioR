
# DNF

# Read in the data
source('http://grimshawville.byu.edu/Tennis2018.R')

# Create the different factors for comparison
tennis$Tournament <- factor(tennis$Tournament)
tennis$Tournament <- relevel(tennis$Tournament, "Masters")
tennis$Gender <- factor(tennis$Gender)
tennis$Gender <- relevel(tennis$Gender, "W")
tennis$Surface <- factor(tennis$Surface)
tennis$Surface <- relevel(tennis$Surface, "Hard")
tennis$Round <- factor(tennis$Round)
tennis$Round <- relevel(tennis$Round, "1st Round")
tennis$Best.of <- factor(tennis$Best.of)
tennis$Best.of <- relevel(tennis$Best.of, "5")

# Create the train and test data sets
set.seed(2112)
dim(tennis)
train_ind <- sample(12914, 10000)
tennis_train <- tennis[train_ind,]
tennis_test <- tennis[-train_ind,]

# Create a boxplot
par(mfrow = c(1,2))
boxplot(WRank ~ DNF, data = tennis_train, xlab = "Winner Rank")
boxplot(LRank ~ DNF, data = tennis_train, xlab = "Loser Rank")

# Tables of proportions
prop.table(table(tennis_train$Gender, tennis_train$DNF), margin = 1)
prop.table(table(tennis_train$Tournament, tennis_train$DNF), margin = 1)
prop.table(table(tennis_train$Round, tennis_train$DNF), margin = 1)
prop.table(table(tennis_train$Best.of, tennis_train$DNF), margin = 1)

#Create contingency plots with categorical variables
prop.table(table(tennis$Gender, tennis$DNF))
prop.table(table(tennis$Tournament, tennis$DNF))
prop.table(table(tennis$Surface, tennis$DNF))
prop.table(table(tennis$Best.of, tennis$DNF))
prop.table(table(tennis$Round, tennis$DNF))

# Define response Variable: DNF = 1 if match ended with loser retiring or walkover
# Explanatory variables:
# Gender, Tournament, Surface, Round, Best.of
# WRank, LRank

#Model
# log(P(DNF)/P(not DNF)) tennis = beta0 + beta1 Men + beta2 GrandSlam + 
#   beta3 Clay + beta4 Grass + beta5 2nd Round + beta6 3rd Round + beta7 4th Round + 
#   bet8 QuarterFinals + beta9 SemiFinals + beta10 Finals + beta11 Best of 3 +
#   beta12 WRank + beta13 LRank

tennis_out <- glm(DNF ~ Gender + Tournament + Surface + Round + Best.of + 
                  WRank + LRank,
                  data = tennis_train,
                  family = "binomial")

summary(tennis_out)

# Difference between Men and Women
# Interpretation:
# log-odds: men DNF more often than women holding all else constant
# change in odds:
exp(coef(tennis_out))
# Holding all else constant men have a .65% increase in odds of DNF compared to women
# 95% CI in change in odds
exp(confint(tennis_out)[-1,])

# No statistically significant difference between5 women and men after adjusting for all other factors
# compute the p-value for the LRT test
tennis_red_gen <- glm(DNF ~ Tournament + Surface + Round + Best.of + WRank + LRank,
                  data = tennis_train,
                  family = "binomial")

anova(tennis_red_gen, tennis_out, test="Chisq")

# Tournament Type (Grand Slam, Masters)
# log-odds: There is decrease of 0.6 in the log odds ratio of DNF for a Grand Slam compared to a Masters
# Odds-ratio: Holding all else constant, there is an estimated 47% decrease in the likelihood of dropping 
#             out of a tennis match when one is participating in a Grand Slam than in a Masters 
#             (95% CI: 26%, 48%)

# Graphic of the effect
# Good:
plot( c(0,1), c(0, -0.566), pch=19, cex = 2.5, xlab = "", ylab = "Partial logit(DNF)", 
      axes=FALSE, xlim=c(-0.5,1.5), ylim=c(-0.7, 0.1))
axis(2)
axis(1, c(0,1), c("Masters", "Grand Slam"))
box()

# Better:
plot( c(0,1), c(0, -0.566), pch=19, cex = 2.5, xlab = "", ylab = "Partial logit(DNF)", 
      axes=FALSE, xlim=c(-0.5,1.5), ylim=c(-1.1, 0.1))

# 95% CI
arrows(1, -1.0030,1, -.0312, code=3, angle=90)
axis(2)
axis(1, c(0,1), c("Masters", "Grand Slam"))
box()


#Best of 3 vs Best of 5
# Holding all else constant, there is a 67% decrease in the likelihood of dropping out of a tennis match 
#   when they are in a best of 3 match compared to a best of 5

#Test Ho: round has no statistically significant effect of DNF

tennis_red_match <- glm(DNF ~ Gender+ Tournament + Surface + Best.of + WRank + LRank,
                        data = tennis_train,
                        family = "binomial")
anova(tennis_red_match, tennis_out, test = "Chisq")

#Test Ho: no difference between surfaces on DNF
tennis_red_surface <- glm(DNF ~ Gender + Tournament + Round + Best.of + WRank + LRank,
                          data = tennis_train,
                          family = "binomial")

anova(tennis_red_surface, tennis_out, test = "Chisq")

# For an increase of 1 in winner's rank, we estimate the odds of 
#     DNF will have an increase of 0.2% holding all else constant

#Outline: Top 3
# 1. Gender
# 2. Surface
# 3. Best of 3

# Top: Gender

# Headline: Are women more reliable than men?

#Predict probability and 95% CI 
predict(tennis_out, newdata = data.frame(Gender="M", 
                                         Tournament="GrandSlam", 
                                         Round = "1st Round", 
                                         Surface="Hard", 
                                         Best.of="5", 
                                         WRank=50, 
                                         LRank=500), 
        type = "response")

logit_hat <- predict(tennis.out, newdata = data.frame(Gender="M", 
                                                      Tournament="GrandSlam", 
                                                      Round = "1st Round", 
                                                      Surface="Hard", 
                                                      Best.of="5", 
                                                      WRank=50, 
                                                      LRank=500), 
                     type = "link", 
                     se=TRUE)

logit_L <- logit_hat$fit - 1.96*logit_hat$se
logit_U <- logit_hat$fit + 1.96*logit_hat$se
phat_L <- exp(logit_L)/(1+exp(logit_L))
phat_U <- exp(logit_U)/(1+exp(logit_U))

#ROC
#install.packages("ROCR")
library(ROCR)
tennis_pred<-prediction(predict(tennis_out, type="response"), tennis_train$DNF)
tennis_perf<-performance(tennis_pred,measure = "tpr", x.measure = "fpr")
tennis_test_pred <- prediction(predict(tennis_out, newdata = tennis_test, type="response"), 
                               tennis_test$DNF)
tennis_test_perf<-performance(tennis_test_pred,measure = "tpr", x.measure = "fpr")

plot(tennis_perf, xlab="1-specificity", ylab="sensitivity", main="ROC curve")
abline(0,1,col = "gray")
plot(tennis_test_perf, add=TRUE, col = "red")

performance(tennis_pred, measure = "auc")
performance(tennis_test_pred, measure = "auc")


