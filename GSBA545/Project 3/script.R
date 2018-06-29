setwd('./Desktop/USC/s1/GSBA545/project 3')
library(forecast)
#A
data1 = read.csv('data1.csv')[,1:2]
data1$Value = data1$Value*1000000
#1
m1 = lm(data =data1,Value~Mission)
summary(m1)
#slope: 52350435, t = 10.81 ,  significant

####manual####
#coef(m1) / sqrt(diag(vcov(m1)))

df = nrow(data1)-2
resid(m1)
sse = sum(resid(m1)^2)
sx = sum((data1$Mission-mean(data1$Mission))^2)
sb1 = sqrt(sse/df)/sqrt(sx)
tb1 = m1$coefficients[2]/sb1
pt(tb1,df=df)
##############

#intervals
#https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
#https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval

#2
#CI
new_data = data.frame(Mission=25)
#predict(m1, newdata = new_data, interval = 'confidence',level=0.95)[2:3]
confint(m1, 'Mission', level=0.95)
########manual#######
mse = sqrt(sse/df)
xse = (1+1 / (df+2) + (25-mean(data1$Mission))^2/ ((df+1)*sx))
qt(0.025,df)
predict(m1, newdata = new_data)+c(1,-1)*qt(0.025,df)*sqrt(mse*xse)
##################

#3
#PI
predict(m1, newdata = new_data, interval = 'prediction',level=0.95)[1:3]

#B
library(dplyr)
delay = read.csv('delay.csv')
data2 = left_join(data1,delay[,c(1,5)],by = c('Mission'='Number'))
data2$Delay[is.na(data2$Delay)]=0
#using all data points
for (i in (1:24)){
  data2$avg[i] = cumsum(data2$Delay[1:i])[i]/i
}

#4
#4a
m = lm(data = data2[-(1:5),],Value~avg)
summary(m)#-88
#4b
confint(m, 'avg', level=0.95)

#5
new_data2 = data.frame(avg=0)
m
predict(m, newdata = new_data2, interval = 'confidence',level=0.95)

#6
m2.0 = lm(data=data2,Value~avg)#R-squared:  0.001106
m2.0 = lm(data=data2[-(1:5),],Value~avg)#R-squared:  0.4721
m2.0 = lm(data=data2[-(1:11),],Value~avg)#R-squared:  0.8743
summary(m2.0)


#C
pits = read.csv('pits.csv')
pits$Flight.time..minutes.[1] = 49
names(pits)
#7
r = cor(pits$Flight.time..minutes.,pits$Depth.of.pit..inches.)
# -0.4282249
t = r/sqrt((1-r^2)/(nrow(pits)-2))
pt(t,df=nrow(pits)-2)
#insig
#significant #http://janda.org/c10/Lectures/topic06/L24-significanceR.htm



#8

#given zero failures, the (1-a) x 100% confidence limit is 1-[a^(1/n)]
1-(0.1^(1/240))
2.3/240
#9
1-pbinom(1,size=5,prob=2.3/240)


#10
p = 6/48
p+qnorm(c(0.95,0.05))*sqrt(p*(1-p)/48)
#11
1-pnorm(0.95,mean=0.2883,sd=0.1879)
#12
#https://faculty.elgin.edu/dkernler/statistics/ch09/9-3.html
sqrt(
  (5*(0.1879^2))/qchisq(c(0.05,0.95),5)
)

bigsd = function(s){
  sqrt(
    (5*(s^2))/qchisq(0.025,5)
  )
}

#part2

#13 #critical pit 0.95
#CA
#The chance that the fore panel has two “zones” leak and that the aft panel also has two “zones” leak is small enough to be ignored.
#DOUBTFUL
#why divivded by 5?
#if not divided
# P(fore panel has “pit”) * P(pit in fore panel > 0.95”) * P(aft panel has “pit”) * P(aft in fore panel > 0.95”) 


#onebig
pits %>%
  mutate(front=ifelse(grepl('fore',Mission),1,0))%>%
  group_by(front)%>%
  summarise(sd=sd(Depth.of.pit..inches.),avg=mean(Depth.of.pit..inches.))
bigsd(0.1928730)
bigsd(0.1258306)
# P(fore panel has “pit”) * P(pit in fore panel > 0.95”) * P(aft panel has “pit”) * P(aft in fore panel > 0.95”) 
(3/24)*(1-pnorm(0.95,mean=0.3966667,sd=0.1258306))*(3/24)*(1-pnorm(0.95,mean=0.1800000,sd=0.1928730)) #2.800141e-12
#very low, acceptable
1-pbinom(0,size=150,prob=2.800141e-12) #4.200211e-10
#if conduct 150 missions, 4.200211e-10 there will be at least a crash  


(3/24)*(1-pnorm(0.95,mean=0.3966667,sd=0.31))*(3/24)*(1-pnorm(0.95,mean=0.1800000,sd=0.47)) #2.940618e-05
1-pbinom(0,size=150,prob=2.940618e-05)#0.4%



#################################################merge data#########
table = read.csv('table.csv')[1:24,2:3]
table$Mission = rownames(table)
d = left_join(data1,delay,by = c('Mission'='Number'))
d$Delay[is.na(d$Delay)]=0
d = d[,c(1,2,4,6)]
d$Mission = as.character(d$Mission)
table=left_join(table,d,by='Mission')
table$Code = as.character(table$Code)
ft =read.csv('flight_time.csv')
temp = data.frame(Code=c(as.character(ft$Mission..),as.character(ft$Mission...1)),time=c(ft$time,ft$time.1))
temp$Code = gsub(' ','',as.character(temp$Code))
table=left_join(table,temp,by='Code')
strsplit(as.character(pits$Mission),' ')
library(reshape2)
pits$Mission[1] = as.character(pits$Mission[1])
y = colsplit(pits$Mission," ",c("Code","Pos"))
y[1,] = c('6-E','fore')
y$flight_time = pits$Flight.time..minutes.
y$pressure = pits$Test.pressure..psi.
y$pits = pits$Depth.of.pit..inches.
library(tidyr)
y$fore = ifelse(y$Pos=='fore',y$pits,NA)
y$aft = ifelse(y$Pos=='aft',y$pits,NA)
y$aft[4] = 0.1
y = y[-5,c(-2,-5)]
table=left_join(table,y,by='Code')
table$Date=as.Date(as.character(table$Date),"%m/%d/%Y")
table$flight_time[2]=49




for (i in (1:24)){
  table$avg[i] = cumsum(table$Delay[1:i])[i]/i
}



table$has_pit = ifelse(is.na(table$flight_time),0,1)
table$pit2 = ifelse(is.na(table$pits),0,table$pits)
table$pressure = c(rep(50,7),100,100,rep(200,15))
table = table[,-8]
###################################################
m = lm(data=table[-(1:11),],Value~avg) 
model <- glm(has_pit ~ time,family=binomial(link='logit'),data=table)
summary(model)
m = lm(data=table[!is.na(table$pits),],pits~avg)
sqrt(0.6287)
summary(m)
new = data.frame(time=90)
predict(m, newdata = new, interval = 'prediction',level=0.99)
(0.005)^2


plot(y=table$pits,x=(table$pressure))
write.csv(table,'data.csv')


#14



#	Based on your own look at the data, what is the risk of mission failure be for “Operation Chalk Outline”?  You may use whatever methodology you think appropriate (but you will discuss your methodology in your Microsoft Word writeup).

#factors: whether flight time has an effect on mission risk, the fact that each KCB has two plates,  the fact that the plates on Mission #25 has received the improved testing of plates

#why people want to delay? #I guess there will be a seasonal issue...

#stock price

#Also note that delays longer than one week will give the Customer the option to cancel the mission, resulting in the $349M loss to the Company.   
#If you recommend proceeding with the mission, provide a financial assessment associated with the risk that the Customer rejects your recommendation (and thus requires that you absorb the $349M loss)
#a mission risk of less than 1 in 10,000 is considered “safe”, and a risk of 1 in 100 would be considered “unacceptable risk”; mission risks between 1 in 100 and 1 in 10,000 are evaluated on a case-by-case basis 
#the chance the customer rejects a “use as is” recommendation is “100 * (calculated probability of mission failure)”
#in the case of a failed mission, the Company’s legal liability is capped at $10 million



#15
#You have been asked to provide a cost-benefit analysis for Mission #25.  Your analysis should include (at least) three aspects of the mission:  
#1.the possible benefit for completing the mission as scheduled, 
#profit
401000000-349000000
#stock price #KCB stands for 40% of revenue
#delay

#on time

#2.the costs associated with delaying or cancelling the mission, 
##delay
#loss without competitor

#loss with competitor

##cancel
#loss without competitor
-349000000
#loss with competitor

#3.the risk associated with performing the mission as scheduled.  
#Q14

#You should also include a clearly-stated final recommendation.









