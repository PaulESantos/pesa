#' TNRS vectorized
#'
#' @param x  species names vector
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x <- c("Plantago tubuloso", "Plantago tubulosa")
#' tnrs_vec(x)
tnrs_vec <- function(x) {
  tnrs <- TNRS::TNRS(x)
  dplyr::tibble(Name_submitted = x) %>%
    dplyr::left_join(tnrs,
                     by = c("Name_submitted" = "Name_submitted")
    ) %>%
    dplyr::as_tibble()
}
