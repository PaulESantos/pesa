#' Count all missing values in a tibble
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' @param df A tibble or data.frame.
#'
#' @return A tibble displaying the number of missing values in the dataframe.
#' @export
#'
check_na <- function(df) {
  n <- function(x) {
    sum(is.na(x))
  }
  df <- df %>%
    dplyr::summarise_all(list(~n(.)))

  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.) %>%
    purrr::set_names(c("vars", "amount_na")) %>%
    dplyr::filter(amount_na > 0)
  return(t_df)
}
