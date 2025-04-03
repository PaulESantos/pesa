utils::globalVariables(c(".", "amount_na", "%+%", "id", "data",
                         "amount", "percent",
                         ".TNRS_base", "tnrs", "lcvp",
                         "open", "index", "group"))
#' Count all missing values in a tibble
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param df A tibble or data.frame.
#'
#' @return A tibble displaying the number of missing values in the dataframe.
#'
# check_na <- function(df) {
#   n <- function(x) {
#     sum(is.na(x))
#   }
#   df <- df |>
#     dplyr::summarise_all(list(~n(.)))
#   t_df <- data.table::transpose(df)
#   colnames(t_df) <- rownames(df)
#   rownames(t_df) <- colnames(df)
#   t_df <- tibble::rownames_to_column(t_df) |>
#     tibble::as_tibble() |>
#     purrr::set_names(c("vars", "amount_na")) |>
#     dplyr::filter(amount_na > 0)
#   return(t_df)
# }
