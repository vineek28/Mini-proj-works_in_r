Ap <- AirPassengers

View(Ap)
str(Ap)
ts(Ap, frequency = 12, start = c(1949,1))

decompo <- decompose(Ap)
plot(
  decompo$figure,
  type =  'b',
  xlab = 'months',
  ylab = 'Season',

)

plot(decompo)

library(forecast)
model <- auto.arima(Ap)
model
attributes(model)


acf(model$residuals, main = "Vineeth gpa _ 9.2")
pacf(model$residuals, main = "Vineeths gpa _ 9.2")



Box.test(model$residuals, lag = 20, type = "Ljung-Box")

hist(
  
  model$residuals, 
  col = "green",
  xlab = 'Error',
  main = "vineeth must get 9.1 this sem",
  freq = F
)

lines(density(model$residuals))


library(ggplot2)
f <- forecast(model, 48)
autoplot(f)
accuracy(f)
