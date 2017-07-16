#' @name llogistic
#' @examples rllogistic(10, 0.5, 2)
#' @export
rllogistic <- function(n, m, b) {
  tta = c(m, b)
  Q = function(tta, u) {
    mu = tta[1]
    s = tta[2]
    (mu * u^(1/s))/((1 - u)^(1/s) * (1 - mu) + u^(1/s) * mu)
  }
  u = runif(n)
  y = Q(tta, u)
  return(y)
}



