\name{malschains.control}
\alias{malschains.control}
\title{Sets and initializes the main parameters of the algorithm}
\usage{malschains.control(popsize=50, ls="cmaes", istep=500, effort=0.5, alpha=0.5, optimum=0,
    threshold=1e-08)
}
\description{This is a function that initializes and sets the parameters of the algorithm. 
It generates a list of parameters, to be used with the \code{\link{malschains}} function.}
\references{Molina, D., Lozano, M., Sánchez, A.M., Herrera, F.
Memetic algorithms based on local search chains for large scale continuous optimisation problems: MA-SSW-Chains
(2011) Soft Computing, 15 (11), pp. 2201-2220. 

Molina, D., Lozano, M., García-Martínez, C., Herrera, F.
Memetic algorithms for continuous optimisation based on local search chains
(2010) Evolutionary Computation, 18 (1), pp. 27-63.}
\arguments{\item{popsize}{The population size of the evolutionary algorithm.}
\item{ls}{The local search method. Currently implementend are \code{cmaes}, \code{sw}, \code{simplex}, and \code{ssw}.
Usually, the \code{cmaes} local search strategy will give good results. However, it does not scale well with the problem size. 
So, if performance is needed, the \code{sw} strategy is a better choice. If the problem is high-dimensional, the \code{ssw} strategy is promising,
which selects randomly 20\% of the variables for optimization.}
\item{istep}{The number of iterations of the local search. I.e., if the local search is started or re-started on an individual, it will be executed for an 
\code{istep} number of iterations. 
This parameter depends on the local search used. For \code{cmaes}, usually an \code{istep} of 300 is
a good choice. For the other local search methods, an \code{istep} of 100 performs better.}
\item{effort}{A value between 0 and 1 which gives the ratio between the amount of evaluations that are used for the local search and
for the evolutionary algorithm, respectively. A higher effort means more evaluations for the evolutionary algorithm. So, if exploration 
of the search space is more important than finding local optima, \code{effort} is to be chosen higher.}
\item{alpha}{The alpha parameter from crossover BLX-alpha. A lower value (< 0.3) reduces diversity, a higher value increases diversity.}
\item{optimum}{The optimum to achieve. The default is zero, as in many minimization problems a value of zero can be considered optimal.}
\item{threshold}{A threshold which defines for the local search how much improvement is considered as no improvement. If this value is chosen 
too low (zero), then the local search will usually always try to improve on the best individual, even if it is already located very close to a local optimum.}
}

