library(gglasso)

set.seed(1234)

X1 <- rnorm(100,4,5)
X2 <- rnorm(100,8,15)
X3 <- rnorm(100,6,7.5)

X5 <- sample(c(1,2,3,4), 100, replace = T)

X4 <- ifelse(X5 == 1, 1, 0)
X6 <- ifelse(X5 == 2, 1, 0)
X7 <- ifelse(X5 == 3, 1, 0)


Y <- 1*X1+ 2*X2 - 3*X3+ 15*X4 + 20*X6 -14*X7 + rnorm(100,0,40)
Y2 <- ifelse(Y < mean(Y), -1, 1)
Y3 <- ifelse(Y < mean(Y), 0, 1)

summary(lm(Y~X1+X2+X3+X4+X6+X7-1))

summary(glm(Y3~X1+X2+X3+X4+X6+X7-1, family = "binomial"))

X <- data.matrix(data.frame(
  X1,
  X2,
  X3,
  X4,
  X6,
  X7
))
X


cv1 <- cv.gglasso(x=X, y=Y, group=c(1,2,3,4,4,4), loss="ls",
                 pred.loss="L1", nfolds = 5)
cv1$lambda.min
cv1$gglasso.fit$beta[,which(cv1$lambda == cv1$lambda.min)]



cv2 <- cv.gglasso(x=X, y=Y, group=1:6, loss="ls",
                 pred.loss="L1", nfolds = 5)
cv2$lambda.min
cv2$gglasso.fit$beta[,which(cv2$lambda == cv2$lambda.min)]



cv3 <- cv.gglasso(x=X, y=Y2, group=c(1,2,3,4,4,4), loss="logit",
                  pred.loss="misclass", nfolds = 5)
cv3$lambda.min
cv3$gglasso.fit$beta[,which(cv3$lambda == cv3$lambda.min)]



cv4 <- cv.gglasso(x=X, y=Y2, group=1:6, loss="logit",
                  pred.loss="misclass", nfolds = 5)
cv4$lambda.min
cv4$gglasso.fit$beta[,which(cv4$lambda == cv4$lambda.min)]


library(ggplot2)

df <- cv1$gglasso.fit$beta
df <- as.data.frame(t(as.data.frame(df)))
df$lambda <- cv1$lambda
row.names(df) <- NULL
df <- as.data.frame(df)

cols <- c("X1"="blue","X2"="red","X3"="orange2", "X4" = "slategray4", "X5" = "slategray4", "X6" = "slategray4")

ggplot(data = df, aes(x = lambda)) +
  geom_line(aes(y = X1, color = "X1")) +
  geom_line(aes(y = X2, color = "X2")) +
  geom_line(aes(y = X3, color = "X3")) +
  geom_line(aes(y = X4, color = "X4")) +
  geom_line(aes(y = X6, color = "X5")) +
  geom_line(aes(y = X7, color = "X6")) +
  scale_colour_manual(name="Variables",values=cols, 
                      guide = guide_legend(override.aes=aes(fill=NA))) + 
  xlim(c(0,25)) +
  ylab("Coefficients")
  




df2 <- cv2$gglasso.fit$beta
df2 <- as.data.frame(t(as.data.frame(df2)))
df2$lambda <- cv2$lambda
row.names(df2) <- NULL
df2 <- as.data.frame(df2)


ggplot(data = df2, aes(x = lambda)) +
  geom_line(aes(y = X1, color = "X1")) +
  geom_line(aes(y = X2, color = "X2")) +
  geom_line(aes(y = X3, color = "X3")) +
  geom_line(aes(y = X4, color = "X4")) +
  geom_line(aes(y = X6, color = "X5")) +
  geom_line(aes(y = X7, color = "X6")) +
  scale_colour_manual(name="Variables",values=cols, 
                      guide = guide_legend(override.aes=aes(fill=NA))) + 
  xlim(c(0,25)) +
  ylab("Coefficients")




df3 <- cv3$gglasso.fit$beta
df3 <- as.data.frame(t(as.data.frame(df3)))
df3$lambda <- cv3$lambda
row.names(df3) <- NULL
df3 <- as.data.frame(df3)


ggplot(data = df3, aes(x = lambda)) +
  geom_line(aes(y = X1, color = "X1")) +
  geom_line(aes(y = X2, color = "X2")) +
  geom_line(aes(y = X3, color = "X3")) +
  geom_line(aes(y = X4, color = "X4")) +
  geom_line(aes(y = X6, color = "X5")) +
  geom_line(aes(y = X7, color = "X6")) +
  scale_colour_manual(name="Variables",values=cols, 
                      guide = guide_legend(override.aes=aes(fill=NA))) + 
  ylab("Coefficients") +
  xlim(c(0,0.0625))


df4 <- cv4$gglasso.fit$beta
df4 <- as.data.frame(t(as.data.frame(df4)))
df4$lambda <- cv4$lambda
row.names(df4) <- NULL
df4 <- as.data.frame(df4)



ggplot(data = df4, aes(x = lambda)) +
  geom_line(aes(y = X1, color = "X1")) +
  geom_line(aes(y = X2, color = "X2")) +
  geom_line(aes(y = X3, color = "X3")) +
  geom_line(aes(y = X4, color = "X4")) +
  geom_line(aes(y = X6, color = "X5")) +
  geom_line(aes(y = X7, color = "X6")) +
  scale_colour_manual(name="Variables",values=cols, 
                      guide = guide_legend(override.aes=aes(fill=NA))) + 
  ylab("Coefficients") +
  xlim(c(0,0.0625))







# Temps de calcul



library(rbenchmark)


benchmark(
  "lm" = {
    lm(Y~X1+X2+X3+X4+X6+X7-1)
  },
  
  "glm" = {
    glm(Y3~X1+X2+X3+X4+X6+X7-1, family = "binomial")
  },
  
  "lasso_classique_Y_continu" = {
    cv.gglasso(x=X, y=Y, group=1:6, loss="ls",
               pred.loss="L1", nfolds = 5)
  },
  "lasso_groupé_Y_continu" = {
    cv.gglasso(x=X, y=Y, group=c(1,2,3,4,4,4), loss="ls",
               pred.loss="L1", nfolds = 5)
  },
  "lasso_classique_Y_binaire" = {
    cv.gglasso(x=X, y=Y2, group=1:6, loss="logit",
               pred.loss="misclass", nfolds = 5)
  },
  "lasso_groupé_Y_binaire" = {
    cv.gglasso(x=X, y=Y2, group=c(1,2,3,4,4,4), loss="logit",
               pred.loss="misclass", nfolds = 5)
  }
  
)











