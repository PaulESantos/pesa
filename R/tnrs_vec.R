#' TNRS vectorized
#'
#' @param x  species names vector
#'
#' @return a tibble
#' @export
tnrs_vec <- function(x) {
  tnrs <- pesa::TNRS(x)
  dplyr::tibble(Name_submitted = x) %>%
    dplyr::left_join(tnrs,
                     by = c("Name_submitted" = "Name_submitted")
    ) %>%
    dplyr::as_tibble()
}
