#' move xs stored in a list into the environment
#'
#' @param x a list
#'
#' @return object stored in x in the current environment
#' @export
#'
#' @examples
#' x <- list(
#'   a = c(1, 2),
#'   b = "test"
#' )
unpack_list <- function(x) {
  for (.x in names(x)) {
    assign(value = x[[.x]], x = .x, envir = parent.frame())
  }
}
