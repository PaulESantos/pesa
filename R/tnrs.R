#' tnrs
#'
#' @param df A data.frame
#' @param specie Species column name
#'
#' @return a tibble
#' @export
#'
#' @examples
#' #df %>% tnrs(specie)
tnrs <- function(df, specie){
  if(unique(class(df) %in% c("tbl_df", "tbl", "data.frame")) == TRUE){

    df %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::group_nest(id, {{specie}}) %>%
      dplyr::mutate(tnrs = purrr::map({{specie}}, ~TNRS::TNRS(.))) %>%
      tidyr::unnest(c(data, tnrs)) %>%
      dplyr::select(-id)

  }
  else{
    message(crayon::green(crayon::underline("Unsupported format")))
  }
}
