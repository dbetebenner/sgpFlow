#' @title sgpFlow: Tools for Generating sgpFlow Trajectories 
#' @description 
#' The `sgpFlow` package provides tools for generating sgpFlow trajectories based on longitudinal student data. 
#' It includes utilities for computing growth percentile trajectories, handling cohort-specific analyses, perturbing scores based on 
#' Conditional Standard Error of Measurement (CSEM), and managing state-specific configurations.
#' 
#' The package is designed for scalability and efficiency when working with large datasets, integrating features such as projection spline matrices, 
#' growth distribution customization, and flexible trajectory simulation.
#' 
#' @details
#' \tabular{ll}{
#'   Package: \tab sgpFlow \cr
#'   Type: \tab Package \cr
#'   Version: \tab 0.0-0.992 \cr
#'   Date: \tab 2025-5-12 \cr
#'   License: \tab MIT \cr
#'   LazyLoad: \tab yes \cr
#' }
#'  
#' **Main Features:**
#' - **Trajectory Generation:** Tools like \code{\link[sgpFlow]{sgpFlowTrajectories}} enable computation of student growth trajectories over multiple iterations.
#' - **Wide and Long Data Support:** Utilities like \code{\link[sgpFlow]{getWideData}} allow seamless conversion between data formats.
#' - **State-Specific Configurations:** Embedded configurations (\code{sgpFlowStateData}) facilitate state-specific SGP analyses.
#' - **Simulation Support:** Integrates CSEM perturbation methods for variability simulation.
#' 
#' **Embedded Data:**
#' The package includes the \code{sgpFlowStateData} object, which provides state-specific meta-data, including:
#' - **Achievement Data:** Contains information such as CSEM values required for perturbation.
#' - **Projection Matrices:** State-specific projection spline matrices for modeling student growth.
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   # Generate SGP trajectories
#' DEMO_Percentile_Trajectories_Cohort_50_LIST <- sgpFlowTrajectories(
#'                                            long_data = sgpData::sgpData_LONG,
#'                                            state = "DEMO",
#'                                            sgpFlow.config = sgpFlow.config,
#'                                            growth.distribution = "50",
#'                                            projection.splineMatrices = projection.splineMatrices)
#'   # Access the first iteration results
#'   print(DEMO_Percentile_Trajectories_Cohort_50_LIST[[1]])
#' }
#' }
#' @seealso 
#'  \code{\link[sgpFlow]{sgpFlowTrajectories}}, 
#'  \code{\link[sgpFlow]{getPercentileTrajectories}}, 
#'  \code{\link[sgpFlow]{getWideData}}, 
#' @name sgpFlow-package
#' @keywords package
#' @docType package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @title State-Specific Meta-Data for SGP Flow Analyses
#' @description 
#' The `sgpFlowStateData` object contains state-specific configurations and meta-data required for SGP analysis. It facilitates tailored analyses by providing:
#' 
#' - Conditional Standard Error of Measurement (CSEM) values for score perturbation
#' - Achievement level data for state-specific assessments
#' - Projection spline matrices for modeling student growth
#' 
#' @details 
#' This data is critical for functions such as:
#' - \code{\link[sgpFlow]{sgpFlowTrajectories}}: Computes percentile trajectories for student cohorts.
#' - \code{\link[sgpFlow]{getPercentileTrajectories}}: Generates detailed growth percentile projections.
#' 
#' **Contents:**
#' - `Achievement`: A list containing CSEM values and metadata for state-specific achievement.
#' - `ProjectionMatrices`: State-specific projection spline matrices used for growth modeling.
#' 
#' The `sgpFlowStateData` object is preloaded into the package and is automatically accessed by relevant functions.
#' 
#' @examples 
#' \dontrun{
#' # Access state-specific CSEM values
#' print(sgpFlowStateData[["NY"]][["Achievement"]])
#' 
#' # Access projection matrices
#' print(sgpFlowStateData[["NY"]][["ProjectionMatrices"]])
#' }
#' @seealso 
#'  \code{\link[sgpFlow]{sgpFlowTrajectories}}, 
#'  \code{\link[sgpFlow]{getPercentileTrajectories}}, 
#'  \code{\link[sgpFlow]{getWideData}}
#' @docType data
#' @name sgpFlowStateData
#' @usage sgpFlowStateData
#' @format A named list containing state-specific meta-data.
#' @keywords datasets
NULL
