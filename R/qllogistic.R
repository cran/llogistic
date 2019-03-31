#' @name llogistic
#' @examples qllogistic(0.2, 0.5, 2)
#' @export
qllogistic = function(p, m, phi, lower.tail = TRUE, log.p = FALSE) {
  q = (m * p^(1/phi))/((1 - p)^(1/phi) * (1 - m) + p^(1/phi) * m)
  if (lower.tail == FALSE) {
    q = 1 - q
  }
  if (log.p == TRUE) {
    q = log(q)
  }
  return(q)
}
