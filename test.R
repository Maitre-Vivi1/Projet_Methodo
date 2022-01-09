library(glmnet)
library(ggplot2)

data("mtcars")


mtcars$gear <- as.factor(mtcars$gear)


df <- mtcars[,c(1,4,6,10)]
df$gear4 <- ifelse(mtcars$gear == 4, 1, 0)
df$gear5 <- ifelse(mtcars$gear == 5, 1, 0)


summary(lm(wt ~ mpg + hp + gear4 + gear5, data = df))




X <- data.matrix(df[,-3])


cv_fit <- cv.glmnet(X, df$wt, alpha = 1)
cv_fit
plot(cv_fit)



fit <- glmnet(X, df$wt, alpha = 1)
coef(fit, s = cv_fit$lambda.min)
plot(fit)


cv_fit$lambda










