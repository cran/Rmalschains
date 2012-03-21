library(Rmalschains)

rastrigin <- function(x) {
  
  dimension <- length(x)
  
  res <- 0.0
  for (i in 1:dimension) {
    res <- res + (x[i]*x[i] - 10.0*cos(2.0*pi*x[i]) + 10.0)
  }

  #print(paste("fitness:", res, sep=""))
  #res <- res - 330
  res 
}

res.rastrigin1 <- malschains(rastrigin, lower=seq(-1.0, -1.0, length=30), upper=seq(1.0, 1.0, length=30), maxEvals=50000, 
    control=malschains.control(effort=0.8, alpha=0.3, popsize=20, istep=100, ls="simplex"))


res.rastrigin2 <- malschains(rastrigin, lower=seq(-1.0, -1.0, length=30), upper=seq(1.0, 1.0, length=30), maxEvals=50000, 
    initialpop = seq(0.1, 0.1, length=30), control=malschains.control(popsize=50, istep=300, ls="cmaes"))

res.rastrigin3 <- malschains(rastrigin, lower=seq(-1.0, -1.0, length=30), upper=seq(1.0, 1.0, length=30), maxEvals=50000, 
    control=malschains.control(effort=0.8, alpha=0.3, popsize=50, istep=100, ls="sw"))

res.rastrigin1
res.rastrigin2
res.rastrigin3

