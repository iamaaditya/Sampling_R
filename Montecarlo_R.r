#Monte Carlo simulation 
#aaditya - 26/10/2012
 
######################################
StartEq = 100
SuccessPcnt = 61.54
SuccessAvg = 2444.78
FailureAvg = 1667.37
Iterations = 1000
NoOfEvents = 100
Rf = 1
#####################################
eq<-rep(NA, NoOfEvents)
 
doMCRun <- function() {
  eq[1] <- StartEq
  for(i in 2:NoOfEvents) {
    if(runif(1,0,100)< SuccessPcnt)
      pl= SuccessAvg * Rf
    else
      pl= -1*FailureAvg * Rf
      eq[i] = eq[i-1]  + pl
  }
  return(eq)
}
 
values <- replicate(Iterations, doMCRun())
 
par(xaxs="i")
plot(1:NoOfEvents, rep(NA, NoOfEvents ), 
     xlab="Iterations", ylab="Growth",
     ylim=c(min(values),max(values)),
     xlim=c(1,NoOfEvents), main="Monte Carlo Simulation")
matlines(values,type="l",lty=1)
 
sd1 = sd(values[NoOfEvents,])
mean1 = mean(values[NoOfEvents,])
print(summary(values[NoOfEvents,]))
sdString = paste("SD : ", sd1)
write(sdString, file="")
outputString = paste("With 99.73% confidence the final growth will be between", 
                     mean1 - 3*sd1, " and ", mean1 + 3*sd1) 
write(outputString, file="")