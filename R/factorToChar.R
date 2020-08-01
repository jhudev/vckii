#' Convert columns from "factor" to "character"
#'
#' This function converts all columns of class "factor" into "character"
#' @param data Working dataset for conversion.
#' @keywords convert, factor, character
#' @export
#' @examples
#' test <- iris #< Set working dataset
#' sapply(test, class)
#' testConv <- factorToChar(test)
#' sapply(testConv, class)


factorToChar <- function(data){
  #Check if inputs are correct
  if(!is.data.frame(data)) stop("'data' is not a proper data.frame.")
  if(length(data)==0) stop("'data' is empty")

  tmp <- sapply(data,is.factor)
  if(length(tmp)>=1){
    data[tmp] <- lapply((1:length(tmp))[tmp],function(e) as.character(data[[e]]))
  }
  return(data)
}

