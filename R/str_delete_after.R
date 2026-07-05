#' Delete characters in a string which occur after a specified pattern
#'
#' @description
#' Vectorised over 'string' and 'pattern'.
#' @param string The string from which to extract.
#' @param pattern Pattern to look for. The default interpretation is a regular expression.
#' @param pos Position of the character used as a marker.
#'
#' @return The part of the string before the pattern.
#' @export
#'
str_delete_after <- function(string, pattern, pos = 1) {
  pos_pattern <- stringr::str_locate_all(string = string, pattern = pattern)

  starts <- sapply(pos_pattern, function(m) {
    if (nrow(m) >= pos) {
      rev(m[, "start"])[pos]
    } else {
      NA_integer_
    }
  })

  string_delete <- stringr::str_sub(
    string = string,
    start = 1,
    end = starts - 1
  )

  return(string_delete)
}
