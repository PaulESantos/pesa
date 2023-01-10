#' IUCN red list
#'
#' @param x Vector of species names
#' @param key IUCN api key
#'
#' @return Tibble
#' @export
#'
iucn_table <- function(x, key = NULL){
  if(is.null(key) == TRUE){
    iucn_key <- Sys.getenv("IUCN_REDLIST_KEY")
  }
  else{
    iucn_key <- key
  }
  rredlist::rl_search(x, key = iucn_key)[2] |>
    as.data.frame() |>
    dplyr::rename_all(list(~stringr::str_replace(., "result.", ""))) |>
    dplyr::as_tibble()
}
