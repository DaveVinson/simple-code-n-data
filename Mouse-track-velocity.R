######################## Simple interpolation pedegogy #######################################################
#random variable, 1000 points
t <- runif(1000,1,2)
#compress, smooth, interpolate to 100 points
#note this provides two variables, an $x variable 
#(think of this at each point captured) 
#and then your converted variable $y
tt<- approx(t,n=100)
#plot that shit
plot(tt$x,tt$y)
#########################################################################################################

data = read.table('exp1_mousetrack_velocity.csv', sep=',', header=FALSE) #note this is velocity data
colnames(data) = list('subject','trial','dirs','lang','xrng','rt')

####################### #### creating variables #### ######################################
data$gravity = 0
data[(data$dirs==1 & data$lang==1),]$gravity=0 # incongruent
data[(data$dirs==1 & data$lang==0),]$gravity=1 #congruent
data[(data$dirs==0 & data$lang==1),]$gravity=1 #congruent 
data[(data$dirs==0 & data$lang==0),]$gravity=0 #incongruent 

data$language = 0
data[data$lang==1,]$language = 'forward'
data[data$lang==0,]$language = 'backward'

data$direct=0
data[data$dirs==1,]$direct = 'up'
data[data$dirs==0,]$direct = 'down'

############################################ plots for velocity data ####################################
### congruent v incongruent ###
library(lme4) 
plot(0,0,xlim=c(0,110),ylim=c(0,160),xlab='time',ylab='Velocity' , main='congruency with gravity')
attach(data)
ps = c();
for (i in 6:106){
  yhigh = mean(data[data$gravity==1 ,i]) #congruent
  ylow = mean(data[data$gravity==0,i]) #incongruent
  
  points(i,yhigh,pch=2,col='blue') #congruent
  points(i,ylow,pch=1,lty="solid",type="b",col='red') #incongruent
  
  lmo = lm(data[,i]~gravity,data=data)    
  coefs =data.frame(summary(lmo)$coefficients)    
  coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
  ps = c(ps,coefs$p[2]) 
}
points((ps>.05)*(-250)-150,1:length(ps),cex=1,pch=8)
#points((ps<.05),1:length(ps),cex=1,pch=8) #??????


#########################################################################################################
### all conditions ####

plot(0,0,xlim=c(75,110),ylim=c(0,150),xlab='X-coordinate',ylab='Y-coordinate' , main='Language and direction, all conditions')
for (i in 6:106){
  yhigh = mean(data[data$lang==1&data$dirs==0 ,i]) #forward and down - congruent
  ylow = mean(data[data$lang==1&data$dirs==1,i]) #forward up- incongruent
  yl = mean(data[data$lang==0&data$dirs==1 ,i]) #backward up - congruent
  yh = mean(data[data$lang==0&data$dirs==0 ,i]) #backward down - incongruent 
  
  points(i,yhigh,pch=1,col='blue') #forward and down - congruent
  points(i,ylow,pch=2,col='red') #forward up- incongruent
  points(i,yl,pch=2,col='green') #backward up - congruent
  points(i,yh,pch=1, lty="solid", col='black') #backward down - incongruent 
}

