Y <- rnorm(100)
X1 <- Y + rnorm(100, 0, 0.25)
X2 <- Y + rnorm(100, 0, 0.25)
X3 <- Y + rnorm(100, 0, 0.25)
X4 <- Y + rnorm(100, 0, 0.25)
X5 <- rnorm(100, 0, 0.25)
X6 <- rnorm(100, 0, 0.5)
X7 <- rnorm(100, 0, 0.75)
X8 <- rnorm(100, 0, 1)
X9 <- rnorm(100, 0, 1.25)

X <- matrix(data = c(X1, X2, X3, X4, X5, X6, X7, X8, X9), nrow = 500, ncol = 9)

df <- data.frame(Y, X1, X2, X3, X4, X5, X6, X7, X8, X9)


summary(lm(Y~., df))


library(glmnet)
library(ggplot2)
abc <- glmnet(df[,2:10],Y, alpha = 1) # lambda = 0.02810677
plot(abc)

summary(abc)
G <- abc$beta[,1:62]

G <- t(G)
G2 <- list(G[,2], G[,3], G[,4], G[,5], G[,6], G[,7], G[,8], G[,9])

plot(y = G[,1], x = abc$lambda, "l")
lapply(G2, lines, x = abc$lambda)

ggplot(as.data.frame(cbind(G,abc$lambda)), aes()) +                
  geom_line(aes(y=y1), colour="red") + 
  geom_line(aes(y=y2), colour="green")


abc <- cv.glmnet(X,Y, alpha = 1)
abc$lambda.1se





