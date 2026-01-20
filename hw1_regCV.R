library(ISLR)
library(tidyverse)

head(Wage)

# MSE of full model
full_model <- lm(wage ~ age+race+education+jobclass, data=Wage); summary(full_model)
yhats <- predict(full_model, Wage)
full_MSE <- mean((Wage$wage - yhats)^2)

# MSE of single split
set.seed(63689)
single_split <- sample(1:nrow(Wage), 9*nrow(Wage)/10)
wage_train_1 <- Wage[single_split, ]
wage_test_1 <- Wage[-single_split, ]
single_model <- lm(wage ~ age+race+education+jobclass, data=wage_train_1)
single_pred <- predict(single_model, newdata=wage_test_1)
single_MSE <- mean((wage_test_1$wage - single_pred)^2)

# this line generated with AI assistance
folds <- cut(seq(1,nrow(Wage)), breaks=10, labels=FALSE)
fold_MSEs <- c()

for (i in 1:10){
  wage_train <- Wage[which(folds!=i), ]
  wage_test <- Wage[which(folds==i), ]
  fold_model <- lm(wage~age+race+education+jobclass, data=wage_train)
  pred <- predict(fold_model, newdata=wage_test)
  fold_MSE <- mean((wage_test$wage - pred)^2)
  fold_MSEs <- c(fold_MSEs, fold_MSE)
}

print(paste("the MSE for the entire dataset is", full_MSE))
print(paste("the MSE for a single split is", single_MSE))
print(paste("the mean MSE for 10-fold split is",  mean(fold_MSEs)))
