# WARNING - Generated by {fusen} from dev/flat_data_contamination.Rmd: do not edit by hand # nolint: line_length_linter.

#' Get results from Spearman's test
#' 
#' Save rho and p.value from Spearman's test in a tibble
#' 
#' @param x first variable
#' @param y second variable
#' 
#' @importFrom stats cor.test
#' @importFrom dplyr tibble
#'
#' @return tibble rho and p.value from Spearman's test
#' 
#' @export
#' @examples
#' # spearman_test()
spearman_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  tibble(rho = test$estimate, p.value = test$p.value)
}
