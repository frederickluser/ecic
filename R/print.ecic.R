##' @title Print ecic objects
##' 
##' @description Prints an `ecic` model while making attributes invisible.
##'  
##' @param x An `ecic` object.
##' @param ... further arguments
##' @param details logical. Set to TRUE to print background information for 
##' every bootstrap run and Changes-in-Changes model.
##' @return An `ecic` print object.
##' @importFrom stats sd
##' @export
##' 
print.ecic = function(x, ..., details = FALSE) {
  
  if (details == FALSE) {
    print( 
      lapply(x, "[[", 1) 
    )
  } else {
    print( 
      x[1:length(x)]
    ) 
  }
    invisible(x)
}
