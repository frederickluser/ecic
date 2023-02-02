##' @title Summary for a changes-in-changes regression with multiple periods and cohorts
##' 
##' @description Summarizes an `ecic` object by aggregating the bootstrap runs. 
##' Works also in an event-study fashion.
##'  
##' @param object An `ecic` object.
##' @param ... further arguments.
##' @return An `ecic_table` object.
##' @importFrom stats sd
##' @export
summary.ecic = function(object, ...) {

  if(class(object)[1] != "ecic") stop("`object` needs to be a valid ecic object.\n")
  
  es         = attributes(object)[["ecic"]][["es"]]
  periods_es = attributes(object)[["ecic"]][["periods_es"]]
  myProbs    = attributes(object)[["ecic"]][["myProbs"]]
  
  # for average QTEs -----------------------------------------------------------
  if (es == FALSE) { 

    myCoefs = lapply(object, "[[", 1) # extract the coefficients from output list
    object_matrix = do.call(cbind, lapply(myCoefs, function(x) x[names(x) %in% "values"]))

    # aggregate the bootstrap runs
    myBoot = data.frame(
      perc  = myProbs,
      coefs = rowMeans(object_matrix),
      se    = apply(object_matrix, 1, stats::sd)
    )
    
  # for event study results ----------------------------------------------------
  } else { 
    
    myBoot = lapply(0:periods_es, function(e) {
      myCoefs = lapply(lapply(object, "[[", (1)), "[[", e+1)
      object_matrix_es = do.call(cbind, lapply(myCoefs, function(x) x[, "values"]))

      # aggregate the bootstrap runs
      temp = data.frame(
        perc  = myProbs,
        es    = e,
        coefs = rowMeans(object_matrix_es),
        se    = apply(object_matrix_es, 1, sd)
      )
      return(temp)
    })
  }
  
  class(myBoot) = c("ecic_table", class(myBoot))
  attr(myBoot, "ecic_table") = list(
    es         = es,
    periods_es = periods_es,
    myProbs    = myProbs
  )
  
  return(myBoot)
}