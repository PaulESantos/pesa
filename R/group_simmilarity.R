#' Group Similarity Function
#'
#' This function groups elements in a vector based on their similarity using the Jaro-Winkler distance metric.
#'
#' @param vect A character vector containing the elements to be grouped.
#' @param dist The threshold distance below which elements are considered similar. Default is 0.4.
#'
#' @return A tibble with columns "index" (the index of the original vector) and "group" (the group index).
#'
#'
#' @examples
#' group_similarity(c("apple", "apples", "banana", "orange", "applle"), dist = 0.3)
#'
#' @export
group_similarity <- function(vect, dist = 0.4) {
  purrr::map_dfr(vect, ~ {
    i <- which(stringdist::stringdist(., vect, "jw") < dist)
    tibble::tibble(index = i, title = vect[i])
  }, .id = "group") |>
    dplyr::distinct(index, .keep_all = TRUE) |>
    dplyr::mutate(group = as.integer(group))
}
