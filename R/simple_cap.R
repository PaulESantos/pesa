#' Capitalize a string and remove unnecessary white spaces
#'
#' This function takes a string as input and performs the following operations:
#' 1. Removes unnecessary white spaces between words.
#' 2. Capitalizes the first letter of the string and converts the rest to lowercase.
#'
#' @param x The input string to be capitalized and cleaned from spaces.
#'
#' @return A string with the first letter capitalized and unnecessary white spaces removed.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' simple_cap("Miconia    secundifolia  subsp.  Secundifolia")
#' # Output: "Miconia secundifolia subsp. secundifolia"
#' }
#'
#' @export
#'
simple_cap <- function(x) {
  # Split each string into words, remove unnecessary white spaces, and convert to lowercase
  words <- sapply(strsplit(x, "\\s+"), function(words) paste(tolower(words), collapse = " "))

  # Capitalize the first letter of each word
  capitalized <- sapply(strsplit(words, ""), function(word) {
    if (length(word) > 0) {
      word[1] <- toupper(word[1])
    }
    paste(word, collapse = "")
  })

  return(capitalized)
}
