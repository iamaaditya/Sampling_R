#Bootstrap
#aaditya - 26/10/2012
 
######################################
dt <- read.csv("successRatio.csv", stringsAsFactors=F)
## successRatio.csv is file which contains samples eg. 
## SampleValue (Probabilty)
## -0.0455
## -0.0042
## -0.0456
## -0.0035
## 0.0394
## 0.0094
 
StartEq = 100
Iterations = 1000
NoOfEvents = 100
Rf = 1
####################################
 
eq<-rep(NA, NoOfEvents)
 
doBSRun <- function() {
  eq[1] <- StartEq
  for(i in 2:NoOfEvents) {
    eq[i] = eq[i-1]  * (1 + sample(dt$SampleValue,1)) * Rf
  }
  return(eq)
}
 
values <- replicate(Iterations, doBSRun())
 
par(xaxs="i")
plot(1:NoOfEvents, rep(NA, NoOfEvents ), 
     xlab="Iterations", ylab="Growth",
     ylim=c(min(values),max(values)),
     xlim=c(1,NoOfEvents), main="Bootstrap Sampling")
matlines(values,type="l",lty=1)
 
sd1 = sd(values[NoOfEvents,])
mean1 = mean(values[NoOfEvents,])
print(summary(values[NoOfEvents,]))
sdString = paste("SD : ", sd1)
write(sdString, file="")
outputString = paste("With 99.73% confidence the final growth will be between", 
                     mean1 - 3*sd1, " and ", mean1 + 3*sd1) 
write(outputString, file="")