#####################################
##  SEIR functions
##  Vanja Dukic, April 9, 2020
#####################################


############ Basic SEIR ODE function

seir <- function(t,x,parms)
{
  S <- x["S"]
  E <- x["E"]
  I <- x["I"]
  R <- x["R"]
  with(as.list(c(parms)),{
    ds <- -b*I*S
    de <- b*I*S - a*E
    di <- a*E - r*I
    dr <- r*I
    der <- c(ds,de,di,dr)
    list(der)
  })
}

###################### SEIR Solver function

SEIR.determ <- function(parms, initial, time.window, ntime){
  require(deSolve)
  times <- seq(time.window[1], time.window[2], length=ntime)
  as.data.frame(lsoda(initial, times, seir, parms))
}

### example: run the above model as follows
#
# γ [(95% confidence interval (CI)] to be: 0.154 (95% CI=0.0721-0.238) http://jtd.amegroups.com/article/view/36385/html
# Because the incubation period of the SARS-CoV-2 has been reported to be between 2 to 14 days (2,10,11,12), we chose the midpoint of 7 days
# RctSeries <- c(rep(3,20),rep(2,40),rep(1,30),rep(0.8,10))
Rct = 3
tmax = 150
t.seq <- seq(0,25,.1);
N <-100
parms <- c(b=0.154*Rct,a=1/7,r=0.154);
initial <- c(S=(N-1)/N, E=0/N, I=1/N, R=0/N);
time.window <- c(0, tmax);
ntime = 100

seir1 <- SEIR.determ(parms, initial, time.window,ntime)
seir1[seir1<0]<-0
names(seir1)
# "time" "S"    "E"    "I"    "R"
par(mfrow=c(1,1))
matplot(seir1$time, seir1[,2:5],type = 'l',lwd=2)
legend(tmax*.87,0.7, legend = c("S", "E", "I", "R"),
       col=c("black", "red", "green", "blue"), lty=1:4, lwd=2, cex=0.8)
