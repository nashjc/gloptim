\name{glopt}
\alias{glopt}
\title{
  Wrapper for Multiple Global / Stochastic Optimization Solvers
}
\description{
  A routine to call several global optimization methods through a
  single syntax. The structure is intended to be extensible via 
  the function gloptim() which calls single solvers via a unified
  syntax, while the present function is intended to loop over a
  set of such solvers.
}
\usage{

glopt(fn, lb, ub, x0 = NULL, maxiter = NULL, popsize = NULL,
      gr = NULL, incl = NULL, excl = NULL, ...) 
}

\arguments{
  \item{fn}{Nonlinear objective function to be optimized, a scalar function
            that takes a real vector of length n as argument and returns a 
            scalar that is the value of the function at that point.}
  \item{lb, ub}{Lower and upper bounds on the parameters, the same length
                as the variables of the objective function.}
  \item{x0}{Some solvers allow or require an initial value; optional}
  \item{method}{A character string specifying the solver to be applied;
                for the current options see the \bold{Details} below.}
  \item{type}{A characterization of the function -- not used at the moment.}
  \item{gr}{Gradient of the objective functions -- not used at the moment.}
  \item{control}{A list of control parameters;
       see \bold{Details} for more information.}
  \item{maxiter}{maximum number of iterations.}
  \item{popsize}{size of population.}
  \item{incl}{character vector of methods to include.}
  \item{excl}{character vector of methods to exclude.}
  \item{\dots}{Additional arguments passed to the objective function \code{fn}.}
}
\details{
  \code{gloptim} is a wrapper function for stochastic/global solvers available.
  See the \bold{usage} section for the list of solvers in different R packages
  that can be named as methods.

  Argument \code{control} is a list specifing changes to default values of the
  algorithm control parameters. If the minimization process threatens to go into
  an infinite loop, try to set \code{maxiter}.

  \code{glopt} takes many methods and generates a data frame with method name,
  minimum value found and elapsed time. \code{incl} takes a list methods to be
  incuded, if \code{NULL} it takes all available methods. \code{excl} excludes
  methods from the include list
}
\value{
  A dataframe with one row per solver attempted and the following columns:
  \item{method}{The name of the solver method.}
  \item{fminimum}{Value of the objective function at termination of this solver.}
  \item{time}{Time taken to attempt the minimization with this solver.}
  \item{p1...pn}{Parameters of the best estimate of the minimum found
       by the method.}
}
\author{
  Hans W Borchers  <hwborchers@googlemail.com> and \cr
  John C. Nash <nashjc@uottawa.ca>
}
\seealso{
  \code{\link{optim}}
}
\examples{
##  See the tests directory and the vignettes.

glopt(fnRastrigin, c(-1, -1), c(1, 1))
##         method         fmin  time
##  1     deoptim 0.000000e+00 0.059
##  2  cppdeoptim 0.000000e+00 0.053
##  3    deoptimr 0.000000e+00 0.076
##  4       deopt 0.000000e+00 0.114
##  5    simplede 0.000000e+00 0.218
##  6    simpleea 0.000000e+00 0.018
##  7         pso 0.000000e+00 0.224
##  8    hydropso 0.000000e+00 1.449
##  9      direct 0.000000e+00 0.030
##  10     crs2lm 0.000000e+00 0.021
##  11      isres 0.000000e+00 0.026
##  12      stogo 0.000000e+00 0.021
##  13      cmaes 0.000000e+00 0.230
##  14  purecmaes 0.000000e+00 0.248
##  15 malschains 0.000000e+00 0.065
##  16      gensa 7.105427e-15 0.016
##  17      psopt 7.602807e-13 0.088
##  18         ga 8.573345e-08 0.647
##  19       soma 4.606847e-06 0.034
##  20     genoud 2.449659e-05 0.352
##  21    ceimopt 7.420777e-04 0.440
##  22   cmaoptim 9.949591e-01 0.825
##  23     cmaesr 1.339898e+00 0.066
##  24       smco 1.990962e+00 0.011
}
\keyword{ global optimization }
\keyword{ stochastic optimization }

