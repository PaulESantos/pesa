#' Count all missing values in a tibble
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the number of missing values (NA) in each column of a tibble or data.frame,
#' and provides the percentage of missing values relative to the total number of rows.
#'
#' @param df A tibble or data.frame.
#'
#' @return A tibble displaying the number of missing values (`amount_na`) and their percentage (`percent_na`)
#'         for each column with at least one missing value.
#' @export
#'
#' @examples
#' df <- data.frame(
#' col1 = c(1, 2, NA, 4),
#' col2 = c(NA, NA, 3, 4),
#' col3 = c(1, 2, 3, 4)
#' )
#' result <- check_na(df)
#'
check_na <- function(df) {
  if (!is.data.frame(df)) {
    stop("The input must be a data frame or tibble.")
  }

  df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(amount = ~ sum(is.na(.x)),
             percent = ~ mean(is.na(.x)) * 100),
        .names = "{.col} - {.fn}"
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("vars", ".value"),
      names_sep = " - "
    ) |>
    dplyr::filter(amount > 0) |>
    dplyr::mutate(percent = round(percent, 2))

}
