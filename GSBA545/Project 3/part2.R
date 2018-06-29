setwd('./Desktop/USC/s1/GSBA545/project 3')
library(forecast)
data = read.csv('data.csv')
#13
#CA is wrong, for there's

#OneBig is wrong


#risk model at time = 90
df = data.frame(time = c(data$time,data$time),pit=c(ifelse(is.na(data$fore),0,data$fore),ifelse(is.na(data$aft),0,data$aft)))
df$time =df$time
df$time = (df$time)^2+df$time
df$pit = df$pit
df = data.frame(mapply(c,data[!is.na(data$fore),c(8,10)],data[!is.na(data$aft),c(8,11)]))
df = data[!is.na(data$fore),c(8,10)]
df = data[!is.na(data$aft),c(8,11)]

plot(df)
colnames(df)=c('time','pit')
cor(df$time,df$pit)
m = lm(data=df,pit~time)
summary(m)
new=data.frame(time=90^2+90)
predict(m,newdata = new,interval = 'prediction',level=0.95)
0.69^2
1.19535^2



p^2 =

risk = p1*p2*p3*p4

#14
risk

#15
#cancel




