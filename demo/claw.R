library(Rmalschains)

claw <- function(xx) {
  x <- xx[1]
  y <- (0.46 * (dnorm(x, -1, 2/3) + dnorm(x, 1, 2/3)) +
        (1/300) * (dnorm(x, -0.5, 0.01) + dnorm(x, -1,
              0.01) + dnorm(x, -1.5, 0.01)) + (7/300) *
        (dnorm(x, 0.5, 0.07) + dnorm(x, 1, 0.07) + dnorm(x,
              1.5, 0.07)))
  return(y)
}

res.claw <- malschains(function(x) {-claw(x)}, lower=c(-3), upper=c(3), 
                       maxEvals=50000, control=malschains.control(popsize=50, 
                       istep=300, ls="cmaes", optimum=-5))

x <- seq(-3, 3,length=1000)
claw_x <- NULL
for (i in 1:length(x)) claw_x[i] <- claw(x[i])

plot(x,claw_x, type="l")
points(res.claw$sol, -res.claw$fitness, col="red")
