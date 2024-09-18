#' Format P-Values
#'
#' This function formats p-values with specified precision. P-values smaller than 0.001 are formatted as "<0.001".
#'
#' @param pv A numeric value representing a p-value.
#' @param my.digit Integer; the number of decimal places to use (default is 3).
#'
#' @return A character string representing the formatted p-value. The function ensures
#' that the returned string is formatted according to the specified number of decimal places (`my.digit`).
#' For p-values smaller than 0.001, the function returns "<0.001" to indicate
#' statistical significance at a high level. This formatting helps in distinguishing
#' between different levels of statistical significance and can be particularly useful
#' in reporting the results of statistical tests. The format "0.000" is avoided to
#' provide a clear indication of very small p-values, enhancing the interpretability
#' of statistical outputs. For example, a return value of "<0.001" suggests a very
#' strong evidence against the null hypothesis, whereas a value of "0.045" formatted
#' with `my.digit=3` suggests a weaker, but still significant, evidence at the 0.05 level.
#'
#' @examples
#' printp(0.000234)
#' printp(0.00456, my.digit = 2)
#'
#' @export
printp <- function(pv, my.digit=3){
  pv1 <- format(round(pv, my.digit), nsmall = my.digit, justify="left")
  if(pv1=="0.000") pv1 <- "<0.001"
  return(pv1)
}