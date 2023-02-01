#' @title Simulated sample data
#'
#' @description A simulated sample panel data with heterogeneous treatment effects across 
#' cohorts and groups. 
#'
#' @format A simulated data frame with 60,000 rows and 5 columns:
#' \describe{
#'   \item{countyreal}{Unit ID}
#'   \item{first.treat}{Cohort}
#'   \item{year}{Period}
#'   \item{time_to_treat}{Period - Cohort}
#'   \item{lemp}{dependent variable}
#' }
#' @source Simulation data
"dat"