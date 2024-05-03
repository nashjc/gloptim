\name{gloptim}
\alias{gloptim}
\title{
  Wrapper for Several Global Optimization Solvers
}
\description{
  A routine to call a variety of global optimization methods through a
  single syntax. The structure is intended to be extensible.
}
\usage{
gloptim(fn, lb, ub, x0 = NULL,
        method = c("deoptim", "cppdeoptim", "deoptimr",         # **DE**
                   "deopt", "simplede", "simpleea",             # **EA**
                   "gensa", "ga", "genoud", # "rbga"            # **GA**
                   "pso", "psopt", "hydropso", # "psoptim"      # **PSO**
                   "direct", "crs2lm", "isres", "stogo",        # **NLoptr**
                   "cmaoptim", "cmaes", "cmaesr", "purecmaes",  # **CMA-ES**
                   "malschains", "ceimopt",                     # **CE**
                   "smco", "soma"), # "tabusearch"              # --others--
        rand = FALSE, type = NULL,
        g = NULL, gr = NULL, control = list(), ...)
}

\arguments{
  \item{fn}{Nonlinear objective function to be optimized, a scalar function
            that takes a real vector as argument and returns a scalar that is
            the value of the function at that point.}
  \item{lb, ub}{Lower and upper bounds on the parameters, the same length
                as the variables of the objective function.}
  \item{x0}{Some solvers allow or require an initial value; optional}
  \item{method}{A character string specifying the solver to be applied;
                for the current options see the \bold{Details} below.}
  \item{rand}{logical; shall the initial point be randomized; defaults to FALSE,
              that is the initial point be the midpoint of the bounding box.}
  \item{type}{A characterization of the function -- not used at the moment.}
  \item{g}{An additional function that specifies the feasible region,
           x is feasible if \code{g(x) >= 0} -- not used at the moment.}
  \item{gr}{Gradient of the objective functions -- not used at the moment.}
  \item{control}{A list of control parameters;
       see \bold{Details} for more information.}
  \item{\dots}{Additional arguments passed to the objective function \code{fn}.}
}
\details{
  \code{gloptim} is a wrapper function for stochastic/global solvers available.
  See the \bold{usage} section for the list of solvers in different R packages
  that can be named as methods.

  Argument \code{control} is a list specifing changes to default values of the
  algorithm control parameters. If the minimization process threatens to go into
  an infinite loop, try to set \code{maxiter}.

  \describe{
  \item{maxiter}{: maximum number of iterations.}
  \item{popsize}{: size of population.}
  \item{trace}{: 0 for no output, other positive integers for more}
  \item{info}{: logical. Ostensibly for same purpose. (To be checked.)}
  }
  
}
\value{
  A list with the following components:
  \item{xmin}{Best estimate of the parameter vector found by the algorithm.}
  \item{fmin}{value of the objective function at termination.}
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


}
\keyword{ global optimization }
\keyword{ stochastic optimization }

