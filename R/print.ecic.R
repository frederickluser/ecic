##' @title Print ecic objects
##' 
##' @description Prints an `ecic` model while making attributes invisible.
##'  
##' @param x An `ecic` object.
##' @param ... further arguments
##' @return An `ecic` print object.
##' @importFrom stats sd
##' @export
##' 
print.ecic = function(x, ...) {
  print( 
    x[1:length(x)]
  ) 
  invisible(x)
}
