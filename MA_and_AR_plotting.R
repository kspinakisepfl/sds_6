#plot an MA(p) time series w/ ggplot

N <- 200
q <- 0
d <- 0
p <- 3




dat <- arima.sim(n = N, list(order = c(q,d,p), ma = c(-0.5, 0.7, 0.2)), sd = sqrt(1))
MAdf <- data.frame(Time = c(1:length(dat)), Value = dat)
ggplot(MAdf, aes(x = Time, y = Value)) + geom_line(size = 0.75) + 
  theme(axis.text=element_text(size=30),axis.title=element_text(size=28,face="bold"))





#plot an AR(q) time series w/ ggplot

N <- 200
q <- 1
d <- 0
p <- 0




dat <- arima.sim(n = N, list(order = c(q,d,p), ar = c(0.8)), sd = sqrt(1))
ARdf <- data.frame(Time = c(1:length(dat)), Value = dat)
ggplot(ARdf, aes(x = Time, y = Value)) + geom_line(size = 0.75)  + 
  theme(axis.text=element_text(size=30),axis.title=element_text(size=28,face="bold"))


