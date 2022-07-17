#' LCVP plant list
#'
#' @param df tibble
#' @param spp Species column name
#'
#' @return a tibble
#' @keywords internal
#'
#' lcvp_tbl <- function(df, spp = NULL){
#'   df |>
#'     dplyr::nest_by({{spp}}) |>
#'     dplyr::mutate(lcvp = purrr::map({{spp}}, ~lcvplants::lcvp_search(.))) |>
#'     tidyr::unnest(c(data, lcvp)) |>
#'     dplyr::ungroup()
#' }
