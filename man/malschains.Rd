\name{malschains}
\alias{malschains}
\title{Perform optimization with the MA-LS-Chains algorithm}
\usage{
  malschains(fn, lower, upper, dim, maxEvals, trace = TRUE,
    initialpop = NULL, control = malschains.control(),
    seed = NULL, env)
}
\arguments{
  \item{fn}{The function to minimize.}

  \item{lower}{The lower bound (or bounds) of the search
  domain.}

  \item{upper}{The upper bound (or bounds) of the search
  domain.}

  \item{dim}{The dimension of the problem (if \code{lower}
  and \code{upper} are vectors it is not needed).}

  \item{maxEvals}{The maximal number of evaluations of the
  fitness function.}

  \item{trace}{Set/unset the verbose mode.}

  \item{initialpop}{An initial population for the
  evolutionary algorithm can be submitted (as a matrix).
  Here, prior knowledge can be introduced to get better
  results from the algorithm.}

  \item{control}{A list containing the main options of the
  algorithm. See \code{\link{malschains.control}}.}

  \item{env}{The environment in which to evaluate the
  fitness function. If not given, it is generated.}

  \item{seed}{A seed value for the random number
  generator.}
}
\value{
  the function returns a list containing the best
  individual, \code{sol}, and its \code{fitness}.
}
\description{
  This is the main function of the package. It minimizes
  the output of the function fn (for maximization, change
  the sign of the output of fn).
}
\details{
  The output of the function is the following:

  \itemize{ \item \code{EA::PopFitness} The fitness of the
  best, the one at the 1st quartile, the one at the 3rd
  quartile, and the worst individual. \item
  \code{EA::Improvement} Improvement of the individuals at
  the according ranked positions in the population (best,
  1st quartile, 3rd quartile, worst). \item \code{LS} The
  number of the individual which is improved on (in
  braces), its fitness before and after application of the
  LS procedure, and their difference. \item \code{EABest}
  If the best fitness present in the population changed:
  same as \code{LS}. }
}
\references{
  Molina, D., Lozano, M., Sánchez, A.M., Herrera, F.
  Memetic algorithms based on local search chains for large
  scale continuous optimisation problems: MA-SSW-Chains
  (2011) Soft Computing, 15 (11), pp. 2201-2220.

  Molina, D., Lozano, M., García-Martínez, C., Herrera, F.
  Memetic algorithms for continuous optimisation based on
  local search chains (2010) Evolutionary Computation, 18
  (1), pp. 27-63.
}

