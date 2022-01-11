library(gglasso)
library(ggplot2)


df_X = data.frame(
  X1 = X1,
  X2 = X2,
  X3 = X3,
  X4 = X4,
  X6 = X6,
  X7 = X7
)



# Group lasso -------------------------------------------------------------


epsi <- Y - predict(cv1$gglasso.fit, newx = df_X)[,which(cv1$lambda == cv1$lambda.min)]
epsi <- epsi - mean(epsi)/100




lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}



B = 400
list_boot <- list()
for (i in 1:B) {
  list_boot <- lappend(list_boot, c(sample(replace = T, x = epsi, size = 100)))
}


list_Y <- list()
for (i in 1:B) {
  list_Y <- lappend(list_Y, predict(cv1$gglasso.fit, newx = df_X)[,which(cv1$lambda == cv1$lambda.min)] + list_boot[[i]])
}


list_beta <- list()
for (i in 1:B) {
  abc <- cv.gglasso(x=X, y=list_Y[[i]], group=c(1,2,3,4,4,4), loss="ls",
                 pred.loss="L1", nfolds = 100)
  
  list_beta <- lappend(list_beta, abc$gglasso.fit$beta[,which(abc$lambda == abc$lambda.min)])
}





# Lasso classique ---------------------------------------------------------


epsi2 <- Y - predict(cv2$gglasso.fit, newx = df_X)[,which(cv2$lambda == cv2$lambda.min)]
epsi2 <- epsi2 - mean(epsi2)/100



list_boot2 <- list()
for (i in 1:B) {
  list_boot2 <- lappend(list_boot2, c(sample(replace = T, x = epsi2, size = 100)))
}


list_Y2 <- list()
for (i in 1:B) {
  list_Y2 <- lappend(list_Y2, predict(cv2$gglasso.fit, newx = df_X)[,which(cv2$lambda == cv2$lambda.min)] + list_boot[[i]])
}


list_beta2 <- list()
for (i in 1:B) {
  abc <- cv.gglasso(x=X, y=list_Y[[i]], group=1:6, loss="ls",
                    pred.loss="L1", nfolds = 100)
  list_beta2 <- lappend(list_beta2, abc$gglasso.fit$beta[,which(abc$lambda == abc$lambda.min)])
}



# Graphs diagnostiques ----------------------------------------------------
# Group lasso -------------------------------------------------------------



cols <- c("X1"="blue","X2"="red","X3"="orange2", "X4" = "yellow", "X5" = "green", "X6" = "slategray4")

list_beta_moy <- list(c(list_beta[[1]]))
for (i in 2:B) {
  list_beta_moy <- lappend(list_beta_moy,  c( sum(list_beta_moy[[i-1]][1], list_beta[[i]][1]),
                                              sum(list_beta_moy[[i-1]][2], list_beta[[i]][2]),
                                              sum(list_beta_moy[[i-1]][3], list_beta[[i]][3]),
                                              sum(list_beta_moy[[i-1]][4], list_beta[[i]][4]),
                                              sum(list_beta_moy[[i-1]][5], list_beta[[i]][5]),
                                              sum(list_beta_moy[[i-1]][6], list_beta[[i]][6])
                                              ))
}


for (i in 1:B) {
  list_beta_moy[[i]] <- list_beta_moy[[i]]/i
}


Beta_1 <- c()
Beta_2 <- c()
Beta_3 <- c()
Beta_4 <- c()
Beta_5 <- c()
Beta_6 <- c()
for (i in 1:B) {
  Beta_1 <- c(Beta_1, list_beta_moy[[i]][1])
  Beta_2 <- c(Beta_2, list_beta_moy[[i]][2])
  Beta_3 <- c(Beta_3, list_beta_moy[[i]][3])
  Beta_4 <- c(Beta_4, list_beta_moy[[i]][4])
  Beta_5 <- c(Beta_5, list_beta_moy[[i]][5])
  Beta_6 <- c(Beta_6, list_beta_moy[[i]][6])
}


df_plot <- data.frame(
  Beta_1 = Beta_1,
  Beta_2 = Beta_2,
  Beta_3 = Beta_3,
  Beta_4 = Beta_4,
  Beta_5 = Beta_5,
  Beta_6 = Beta_6,
  X = 1:B
)


ggplot(data = df_plot, aes(x = X)) +
  geom_line(aes(y = Beta_1, color = "X1")) +
  geom_line(aes(y = Beta_2, color = "X2")) +
  geom_line(aes(y = Beta_3, color = "X3")) +
  geom_line(aes(y = Beta_4, color = "X4")) +
  geom_line(aes(y = Beta_5, color = "X5")) +
  geom_line(aes(y = Beta_6, color = "X6")) +
  scale_colour_manual(name="Variables",values=cols, 
                      guide = guide_legend(override.aes=aes(fill=NA))) + 
  ylab("Coefficients") +
  xlab("Bootstrap replication")
  


# Lasso -------------------------------------------------------------------



list_beta_moy2 <- list(c(list_beta2[[1]]))
for (i in 2:B) {
  list_beta_moy2 <- lappend(list_beta_moy2,  c( sum(list_beta_moy2[[i-1]][1], list_beta2[[i]][1]),
                                              sum(list_beta_moy2[[i-1]][2], list_beta2[[i]][2]),
                                              sum(list_beta_moy2[[i-1]][3], list_beta2[[i]][3]),
                                              sum(list_beta_moy2[[i-1]][4], list_beta2[[i]][4]),
                                              sum(list_beta_moy2[[i-1]][5], list_beta2[[i]][5]),
                                              sum(list_beta_moy2[[i-1]][6], list_beta2[[i]][6])
  ))
}


for (i in 1:B) {
  list_beta_moy2[[i]] <- list_beta_moy2[[i]]/i
}


Beta_1_2 <- c()
Beta_2_2 <- c()
Beta_3_2 <- c()
Beta_4_2 <- c()
Beta_5_2 <- c()
Beta_6_2 <- c()
for (i in 1:B) {
  Beta_1_2 <- c(Beta_1_2, list_beta_moy2[[i]][1])
  Beta_2_2 <- c(Beta_2_2, list_beta_moy2[[i]][2])
  Beta_3_2 <- c(Beta_3_2, list_beta_moy2[[i]][3])
  Beta_4_2 <- c(Beta_4_2, list_beta_moy2[[i]][4])
  Beta_5_2 <- c(Beta_5_2, list_beta_moy2[[i]][5])
  Beta_6_2 <- c(Beta_6_2, list_beta_moy2[[i]][6])
}


df_plot_2 <- data.frame(
  Beta_1_2 = Beta_1_2,
  Beta_2_2 = Beta_2_2,
  Beta_3_2 = Beta_3_2,
  Beta_4_2 = Beta_4_2,
  Beta_5_2 = Beta_5_2,
  Beta_6_2 = Beta_6_2,
  X = 1:B
)


ggplot(data = df_plot_2, aes(x = X)) +
  geom_line(aes(y = Beta_1_2, color = "X1")) +
  geom_line(aes(y = Beta_2_2, color = "X2")) +
  geom_line(aes(y = Beta_3_2, color = "X3")) +
  geom_line(aes(y = Beta_4_2, color = "X4")) +
  geom_line(aes(y = Beta_5_2, color = "X5")) +
  geom_line(aes(y = Beta_6_2, color = "X6")) +
  scale_colour_manual(name="Variables",values=cols, 
                      guide = guide_legend(override.aes=aes(fill=NA))) + 
  ylab("Coefficients") +
  xlab("Bootstrap replication")



# Biais -------------------------------------------------------------------
# Group Lasso -------------------------------------------------------------

list_beta_moy[[B]] - cv1$gglasso.fit$beta[,which(cv1$lambda == cv1$lambda.min)]

# Lasso -------------------------------------------------------------------

list_beta_moy2[[B]] - cv2$gglasso.fit$beta[,which(cv2$lambda == cv2$lambda.min)]


# IC group lasso ----------------------------------------------------------

coef_beta_1 <- c()
coef_beta_2 <- c()
coef_beta_3 <- c()
coef_beta_4 <- c()
coef_beta_5 <- c()
coef_beta_6 <- c()
for (i in 1:B) {
  coef_beta_1 <- c(coef_beta_1, list_beta[[i]][1])
  coef_beta_2 <- c(coef_beta_2, list_beta[[i]][2])
  coef_beta_3 <- c(coef_beta_3, list_beta[[i]][3])
  coef_beta_4 <- c(coef_beta_4, list_beta[[i]][4])
  coef_beta_5 <- c(coef_beta_5, list_beta[[i]][5])
  coef_beta_6 <- c(coef_beta_6, list_beta[[i]][6])
}

coef_beta_1 <- sort(coef_beta_1)
coef_beta_1[0.025*B] ; coef_beta_1[0.975*B]
coef_beta_2 <- sort(coef_beta_2)
coef_beta_2[0.025*B] ; coef_beta_2[0.975*B]
coef_beta_3 <- sort(coef_beta_3)
coef_beta_3[0.025*B] ; coef_beta_3[0.975*B]
coef_beta_4 <- sort(coef_beta_4)
coef_beta_4[0.025*B] ; coef_beta_4[0.975*B]
coef_beta_5 <- sort(coef_beta_5)
coef_beta_5[0.025*B] ; coef_beta_5[0.975*B]
coef_beta_6 <- sort(coef_beta_6)
coef_beta_6[0.025*B] ; coef_beta_6[0.975*B]


# IC Lasso ----------------------------------------------------------------


coef_beta_1_2 <- c()
coef_beta_2_2 <- c()
coef_beta_3_2 <- c()
coef_beta_4_2 <- c()
coef_beta_5_2 <- c()
coef_beta_6_2 <- c()
for (i in 1:B) {
  coef_beta_1_2 <- c(coef_beta_1_2, list_beta2[[i]][1])
  coef_beta_2_2 <- c(coef_beta_2_2, list_beta2[[i]][2])
  coef_beta_3_2 <- c(coef_beta_3_2, list_beta2[[i]][3])
  coef_beta_4_2 <- c(coef_beta_4_2, list_beta2[[i]][4])
  coef_beta_5_2 <- c(coef_beta_5_2, list_beta2[[i]][5])
  coef_beta_6_2 <- c(coef_beta_6_2, list_beta2[[i]][6])
}

coef_beta_1_2 <- sort(coef_beta_1_2)
coef_beta_1_2[0.025*B] ; coef_beta_1_2[0.975*B]
coef_beta_2_2 <- sort(coef_beta_2_2)
coef_beta_2_2[0.025*B] ; coef_beta_2_2[0.975*B]
coef_beta_3_2 <- sort(coef_beta_3_2)
coef_beta_3_2[0.025*B] ; coef_beta_3_2[0.975*B]
coef_beta_4_2 <- sort(coef_beta_4_2)
coef_beta_4_2[0.025*B] ; coef_beta_4_2[0.975*B]
coef_beta_5_2 <- sort(coef_beta_5_2)
coef_beta_5_2[0.025*B] ; coef_beta_5_2[0.975*B]
coef_beta_6_2 <- sort(coef_beta_6_2)
coef_beta_6_2[0.025*B] ; coef_beta_6_2[0.975*B]






  
