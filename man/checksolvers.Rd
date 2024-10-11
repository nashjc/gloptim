\name{checksolvers}
\alias{checksolvers}
\title{
  Check and install solvers
}
\description{
  Check each solver is installed and offer to install missing ones.
}
\usage{
   checksolvers()
}

\arguments{
   none
}
\value{
  A vector of names of available solvers. Missing ones are removed from the
  'allmethod' default list which is set within this function.
}
\author{
  John C. Nash <nashjc@uottawa.ca>
}
\keyword{ global optimization }
\keyword{ stochastic optimization }

