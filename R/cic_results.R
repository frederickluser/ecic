##' Estimate a changes-in-changes model with multiple periods and cohorts
##'
##' @param object An `ecic` object.
##' @return An `ecic_res` object.
##' @importFrom stats sd
##' @export
cic_summary <- function(object) {

  if(class(object)[1] != "ecic") stop("\"object\" needs to be a valid ecic object.")
  
  es         = attributes(object)[["ecic"]][["es"]]
  periods_es = attributes(object)[["ecic"]][["periods_es"]]
  myProbs    = attributes(object)[["ecic"]][["myProbs"]]
  
  
  if (es == F) { # for average QTEs

    myCoefs <- lapply(object, "[[", 1) # extract the coefficients from output list
    object_matrix <- do.call(cbind, lapply(myCoefs, function(x) x[names(x) %in% "values"]))

    # aggregate the bootstrap runs
    myBoot <- data.frame(
      perc = myProbs,
      coefs = rowMeans(object_matrix),
      se = apply(object_matrix, 1, stats::sd)
    )
  } else { # for event study results
    
    myBoot <- lapply(0:periods_es, function(e) {
      myCoefs <- lapply(lapply(object, "[[", 1), "[[", 1)
      object_matrix_es <- do.call(cbind, lapply(myCoefs, function(x) x[, "values"]))

      # aggregate the bootstrap runs
      temp <- data.frame(
        perc = myProbs,
        es = e,
        coefs = rowMeans(object_matrix_es),
        se = apply(object_matrix_es, 1, sd)
      )
      return(temp)
    })
  }
  
  class(myBoot) = c("ecic_res", class(myBoot))
  attr(myBoot, "ecic_res") = list(
    es = es,
    periods_es = periods_es,
    myProbs = myProbs
  )
  
  return(myBoot)
}