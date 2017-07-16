#' @name llogistic
#' @examples pllogistic(0.7, 0.5, 2)
#' @export
pllogistic = function(q, m, b, lower.tail = TRUE, log.p = FALSE) {
  p = 1/(1 + (m * (1 - q)/(q * (1 - m)))^b)
  if (lower.tail == FALSE) {
    p = 1 - p
  }
  if (log.p == TRUE) {
    p = log(p)
  }
  return(p)
}

