library(dplyr)
#function was built from line 54 to line 139
#math was calculated from line 140 to line 186
#rest are some simulations..
bind = dbinom(c(0:121),121,0.025)
#n=60
return60 = c(seq(240,240+60*(-170),-170),rep(240+60*(-170),61))*1000000
n60 = bind*return60
E60 = sum(n60)
Var60 = sum((return60-E60)^2*bind)
SE60 = sqrt(Var60)
p60 = sum(dbinom(c(0:1),121,0.025))
d60 =  sum(bind[3:122]*return60[3:122])
#n=120
return120 = c(seq(480,480+120*(-170),-170),480+120*(-170))*1000000
n120 = bind*return120
E120 = sum(n120)
Var120 = sum((return120-E120)^2*bind)
SE120 = sqrt(Var120)
p120 = sum(dbinom(c(0:2),121,0.025))
d120 =  sum(bind[4:122]*return120[4:122])
#A
returnA = c(-480,-250,-20,210,rep(210,118))*1000000
EA = sum(bind*returnA)
VarA = sum(((returnA-EA)^2)*bind)
SEA = sqrt(VarA)
pa = sum(dbinom(c(3:121),121,0.025))
da = sum((bind*returnA)[1:3])
#B
returnB = c(rep(-480,4),rep(800,5)*c(1:5)-480,rep(800,113)*5-480)*1000000
EB = sum(bind*returnB)
VarB = sum(((returnB-EB)^2)*bind)
SEB = sqrt(VarB)
pb = sum(dbinom(c(4:121),121,0.025))
db = sum((bind*returnB)[1:4])
#C
8*113 #make sure 904

returnC = c(rep(-480,9),rep(8000,113)*c(1:113)-480)*1000000
EC = sum(bind*returnC)
VarC = sum(((returnC-EC)^2)*bind)
SEC = sqrt(VarC)
pc = sum(dbinom(c(9:121),121,0.025)) #1-pbinom(8,121,0.025)
dc = sum((bind*returnC)[1:9])


#2
dbinom(3,121,0.025)

#3 look at line 122, skip the functions
#are they independent?????????
#monte carlo simulation of different scenarios

###Tier A Tier B Tier C
#a ==1 means buy
SILA = function(a,default) {
  if (a == 1) {
    if (default <=3 ) {
       230*default-480
    } else {690-480}
  }
  else{0}
}
SILB = function(b,default) {
  if (b == 1) {
    if(default >= 4 & default <= 8 ) {
      800*(default-3)-480
    }
    else if (default > 8){
      800*5-480
    } else {0-480}
  }
  else{0}
}
SILC = function(c,default) {
  if (c == 1) {
    if (default >= 9 ) {
      8000*(default-8)-480
    }
    else {0-480}
  }
  else{0}
}

###without default information
playBlind = function(n,a,b,cc) {
  p = sample(c(0.015,0.025,0.035),1,prob = c(0.3,0.4,0.3))
  ins = n
  default = rbinom(1,121,p)
  surplus = -(a+b+cc)*480 + ins*4
  interest = if (surplus >= 0) { 
    surplus*0.0214
  } else {
    surplus*0.07  
  }
  if (default > ins){d=ins}else{d=default}
  #return
  ins*4+(-170)*d+SILA(a,default)+SILB(b,default)+SILC(cc,default)+interest
}
####

#validate playBLind mechanism
v = NULL
for (i in 1:1000) {
  v[i] = sample(c(0.015,0.025,0.035),1,prob = c(0.3,0.4,0.3))
}
hist(v)
####
### value of information (Note I havn't deduct information cost)
playKnown = function(p,n,a,b,c) {
  ins = n
  default = rbinom(1,121,p)
  surplus = -(a+b+c)*480 + ins*4
  interest = if (surplus >= 0) { 
    surplus*0.0214
  } else {
    surplus*0.07  
  }
  if (default > ins){d=ins}else{d=default}
  #return
  ins*4+(-170)*d+SILA(a,default)+SILB(b,default)+SILC(c,default)+interest
}

r = NULL
math = function(p,n,a,b,c) {
  ins = n
  surplus = -(a+b+c)*480 + ins*4
  interest = if (surplus >= 0) { 
    surplus*0.0214
  } else { surplus*0.07 }
  
  for (i in 0:121){
    default = i
    if (default > ins){d=ins}else{d=default}
    #expected return
    r[i+1] = ins*4+(-170)*d+SILA(a,default)+SILB(b,default)+SILC(c,default)+interest
  }
  sum(r*dbinom(c(0:121),121,p))*1000000
}
#####All possible combinations
df = expand.grid(n=c(0:121),a=c(0,1),b=c(0,1),cc=c(0,1))
##### math for Q3
math(0.025,121,1,1,0) #NiKhil

df_math = df
for (i in 1:nrow(df)){
  df_math$return[i] = math(0.025,df[i,1],df[i,2],df[i,3],df[i,4])
}
df_math %>%
  arrange(desc(return)) %>%
  slice(1:10)

#math for Q4 and Q5
df_math15 = df 
df_math25 = df 
df_math35 = df 
for (i in 1:nrow(df)){
  df_math15$return[i] = math(0.015,df[i,1],df[i,2],df[i,3],df[i,4])
  df_math25$return[i] = math(0.025,df[i,1],df[i,2],df[i,3],df[i,4])
  df_math35$return[i] = math(0.035,df[i,1],df[i,2],df[i,3],df[i,4])
}
df_math15 %>%
  arrange(desc(return)) %>%
  slice(1:5)
df_math25 %>%
  arrange(desc(return)) %>%
  slice(1:5)
df_math35 %>%
  arrange(desc(return)) %>%
  slice(1:5)
#information worth?
#no info
nf=math(0.025,121,1,1,0)*0.4+math(0.015,121,1,1,0)*0.3+math(0.035,121,1,1,0)*0.3
#with info
wf=math(0.025,121,1,1,0)*0.4+math(0.015,121,0,0,0)*0.3+math(0.035,0,1,1,0)*0.3 #Nikhil
wf-nf #209.2995

##math for Q6
wfuncertain=
  (math(0.025,121,1,1,0)*0.8+math(0.015,121,1,1,0)*0.1+math(0.035,121,1,1,0)*0.1)*0.4+
  (math(0.025,121,0,0,0)*0.1+math(0.015,121,0,0,0)*0.9)*0.3+
  (math(0.025,0,1,1,0)*0.1+math(0.035,0,1,1,0)*0.9)*0.3
#slightly different from Nikhil's 24.066798
wf-wfuncertain  #21.69768

##math for Q7
#I'm lazy, just look at the simulation
#should be around 0.2313
sum(dbinom(0:3,24,0.086)*dbinom(3:0,97,0.0099))
##7
v7 = NULL
for (i in 1:1000000) {
  if (rbinom(1,24,0.086)+rbinom(1,97,0.0099) ==3) {
    v7[i] = 1
  } else {
    v7[i] = 0
  }
}
mean(v7)
######  simulation for Q3
rt00 = NULL
min00 = NULL
med00 = NULL
max00 = NULL
vector = NULL
for (j in 1:nrow(df)) {
  for (i in 1:1000) {
    vector[i] = playKnown(0.025,df[j,1],df[j,2],df[j,3],df[j,4])
  }
  rt00[j] = summary(vector)[4]
  min00[j] = summary(vector)[1]
  med00[j] = summary(vector)[3]
  max00[j] = summary(vector)[6]
}

ndf00 = cbind(df,rt00,min00,med00,max00)
ndf00 %>%
  arrange(desc(rt00)) %>%
  slice(1:10)


#4  simulation for the value of information
###moron
vector = NULL

rt = NULL
for (j in 1:nrow(df)) {
  for (i in 1:10000) {
    vector[i] = playBlind(df[j,1],df[j,2],df[j,3],df[j,4])
  }
  rt[j] = mean(vector)
}
ndf = cbind(df,rt)
ndf %>%
  arrange(desc(rt)) %>%
  slice(1:15)

###coward
#0.015
rt1 = NULL
for (j in 1:nrow(df)) {
  for (i in 1:1000) {
    vector[i] = playKnown(0.015,df[j,1],df[j,2],df[j,3],df[j,4])
  }
  rt1[j] = mean(vector)
}
ndf1 = cbind(df,rt1)
#0.025
rt2 = NULL
vector = NULL
for (j in 1:nrow(df)) {
  for (i in 1:1000) {
    vector[i] = playKnown(0.025,df[j,1],df[j,2],df[j,3],df[j,4])
  }
  rt2[j] = mean(vector)
}
ndf2 = cbind(df,rt2)
ndf2 %>%
  arrange(desc(rt2)) %>%
  slice(1:10)
#0.035
rt3 = NULL
for (j in 1:nrow(df)) {
  for (i in 1:1000) {
    vector[i] = playKnown(0.035,df[j,1],df[j,2],df[j,3],df[j,4])
  }
  rt3[j] = mean(vector)
}
ndf3 = cbind(df,rt3)

par(mfrow = c(2,2))
plot(rt)
plot(rt1)
plot(rt2)
plot(rt3)
hist(vector)

