#' @title Get the IUCN growth form of a species
#'
#' @description
#' This function takes a species name and returns the IUCN growth form
#'
#' @param sp A character vector of species names
#' @param key A character string for the IUCN API key (optional)
#'
#' @return A tibble with species, species_2, and growth_form
#'
#' @export
#'
#' @examples
#' growth_forms_iucn("Gorilla gorilla")
#'
growth_forms_iucn <-  function (sp, key = NULL) {
  if (is.null(key) == TRUE) {
    iucn_key <- Sys.getenv("IUCN_REDLIST_KEY")
  }
  else {
    iucn_key <- key
  }

  rs <- rredlist::rl_growth_forms(sp, key = iucn_key)

  dplyr::tibble(
    species = sp,
    species_2 =   rs$name,
    growth_form = rs$result$name
  )
}
