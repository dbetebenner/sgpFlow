\name{createLookupTables}
\alias{createLookupTables}
\title{Create SGP Lookup Tables based upon SGP Analyses}
\description{
The \code{createLookupTables} function generates SGP lookup tables for student growth percentile (SGP) analyses based on specified configuration settings and matrices. This function supports single-cohort and super-cohort lookup table types, allowing for flexible table creation based on user-defined parameters.
}
\usage{
createLookupTables(
  state,
  sgp.config,
  parallel.config,
  matrices = NULL,
  lookup_table_types = c("single-cohort", "super-cohort")
)
}
\arguments{
  \item{state}{Acronym indicating state associated with the data for access to \code{sgpFlowMatrices}.}
  
  \item{sgp.config}{Configuration specifying desired grade by content area lookup tables. Note that lookup tables contain all permutations of prior and current scores. 
  Though the function can theoretically produce lookup tables based upon two or more priors, such tables will contain > 10^9 rows for each grade and content area and will exhaust memory on all but the largest systems.
  See examples below for configurations based upon a single prior.}
  
  \item{parallel.config}{A named list with, at a minimum, two elements indicating 1) the BACKEND package to be used for parallel computation and 2) the WORKERS list to specify the number of processors to be used in each major analysis.
  The BACKEND element can be set = to \code{FOREACH} or \code{PARALLEL}.  Please consult the manuals and vignettes for information of these packages!  The \code{\link{analyzeSGP}} help page contains more thorough explanation and examples of the \code{parallel.config} setup.}
  
  \item{matrices}{Specifies the matrices used for lookup table generation. If not provided (NULL), the function attempts to use matrices from the \code{sgpFlowMatrices} package based on the \code{state} parameter.}
  
  \item{lookup_table_types}{Character vector. Types of lookup tables to create. Options include \code{"single-cohort"} and \code{"super-cohort"}. Default is \code{c("single-cohort", "super-cohort")}.}
}
\details{
The \code{createLookupTables} function is designed to generate SGP lookup tables, based upon either single-cohort or super-cohort SGP coefficient matrices using the specified \code{state} coefficient matrices and \code{sgp.config} settings. This function can handle multiple types of configurations and matrices, allowing flexibility in the lookup tables generated. 

The function follows these steps:
\enumerate{
  \item Retrieves the latest matrix year from \code{sgp.config}.
  \item Selects appropriate matrices from the \code{sgpFlowMatrices} package or uses matrices provided as a function argument.
  \item Creates and populates lookup tables based on \code{lookup_table_types}.
  \item Extracts tables from the SGP object and saves them in CSV and RData formats.
}
}
\value{
The function does not return an object but saves lookup tables in the specified file paths within the \code{Data} directory.
}
\examples{
# Example usage of createLookupTables
createLookupTables(
  state = "DEMO",
  sgp.config = demo_sgp_config,
  parallel.config = demo_parallel_config,
  matrices = demo_matrices,
  lookup_table_types = c("single-cohort", "super-cohort")
)
}
\seealso{
\code{\link{abcSGP}}, \code{\link{extractLookupTables}}, \code{\link{createScaleScorePermutations}}
}
\author{
Your Name
}
\keyword{SGP}
\keyword{lookup tables}
