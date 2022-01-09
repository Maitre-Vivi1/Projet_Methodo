library(grplasso)


fit_mtcars <- grplasso(wt ~ hp + mpg + gear, data = df, model = LinReg(), lambda = seq(0.01,20,0.01)[2000:1],
                       nonpen = ~1)

print(fit_mtcars)


plot(fit_mtcars)


View(t(fit_mtcars$coefficients))






