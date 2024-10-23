\name{addMatrices}
\alias{addMatrices}
\title{Function for adding sgpFlow coefficient matrices in sgpFlowMatrices to SGP object.}
\description{
The function addMatrices adds sgpFlow coefficient matrices contained in the sgpFlowMatrices package to the SGPstateData object embedded in the SGP package so baseline SGP analyses can be conducted.
}

\usage{
addMatrices(
    state,
    year,
    type="Single-Cohort",
    add.matrices.to.which.state=NULL)
}

\arguments{
  \item{state}{A character string for the state/assessment abbreviation for the sgpFlow coefficient matrices to be used. Must match value to be used in SGPstateData.
  }
  \item{year}{Year associated with the matrices.
  }
  \item{type}{Type of matrices: Currently either 'Single-Cohort' (default) or 'Super-Cohort'.}
  \item{add.matrices.to.which.state}{A character string for the state/assessment abbreviation where the baseline matrices will be placed in SGPstateData. If not argument is supplied the default is to use the state/assessment abbreviation supplied via the argument \code{state}.
  }
}

\value{Returns an SGP object with matrices embedded in appropriate slot embedded in the position associated with the argument \code{add.matrices.to.state} (defaults to \code{state}).
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\keyword{documentation}
