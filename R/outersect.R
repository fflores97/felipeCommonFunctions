#' Find elements that are not in common
#'
#' @description Reverse of intersect
#' @param x Element 1
#' @param y Element 2
#' @author Felipe Flores
#' @return Sorted vector of elements that are different
outersect <- function(x,y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
