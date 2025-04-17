#' Query IUCN Red List Categories for a List of Species
#'
#' This function takes a character vector or data frame containing species
#' names and returns a tidy tibble with the original names and their
#' corresponding IUCN Red List categories. Species names are cleaned,
#' parsed, and checked to ensure they follow a binomial format before
#' querying the IUCN Red List API.
#'
#' @param splist A character vector of species names or a data frame
#' containing a column with species names.
#' @param api A character string with a valid IUCN Red List API key.
#' This is required to query the IUCN service.
#' @param var_name If `splist` is a data frame, `var_name` indicates
#' the name of the column containing species names.
#'
#' @return A tibble with the original name (`orig_name`), parsed genus
#' and species (`orig_genus`, `orig_species`), and the corresponding
#' IUCN Red List category (`category`). If `splist` is a data frame,
#' all original columns will be returned with the category appended.
#'
#' @details
#' The function performs the following steps:
#' - Standardizes and cleans the input names (removing hybrids, normalizing
#'  format).
#' - Classifies names into genus and species components.
#' - Verifies whether names are binomial; non-binomial names are retained
#'  but not queried.
#' - Queries the IUCN Red List API using genus and species for binomial names.
#' - Joins IUCN category results with the original list.
#'
#' The function uses internal helper functions: `.names_standardize()`,
#'  `.splist_classify()`, `.classify_algo()`, `.transform_split_classify()`,
#'   `.check_binomial()`, `.splist_format_df()`.
#'
#' @importFrom dplyr mutate relocate select arrange row_number bind_rows
#' @importFrom tidyr unnest
#' @importFrom purrr pmap
#' @importFrom stringr str_to_sentence
#'
#' @examples
#' \dontrun{
#' species <- c("Polylepis incana", "Oreopanax aff. mutisianus", "Xylopia sp.")
#' api_key <- "your_api_key_here"
#' iucn_tbl(species, api = api_key)
#' }
#'
#' @export
iucn_tbl <- function(splist,
                     api = NULL,
                     var_name = "scientific_name"){

  splist_output <- .splist_formated(splist = splist)

  splist_class_binomial <- splist_output[splist_output$binomial == "binomial", ]

  splist_class_non_binomial <- splist_output[splist_output$binomial == "non binomial", ]

  if(nrow(splist_class_non_binomial) != 0){
    splist_class_non_binomial$category <- "---"
  }else{
    splist_class_non_binomial
  }

  resultado <- splist_class_binomial |>
    dplyr::mutate(
      iucn_info = purrr::pmap(
        list(orig_genus, orig_species),
        ~ get_iucn_category(genus = ..1, species = ..2, api = api)
      )
    ) |>
    tidyr::unnest(iucn_info) |>
    dplyr::bind_rows(splist_class_non_binomial) |>
    dplyr::arrange(sorter) |>
    dplyr::mutate(orig_name = str_to_simple_cap(orig_name),
                  category = ifelse(is.na(category), "---", category))
  ### otput vars
  if(is.character(splist)){
    var_name <- c("sorter","orig_name", "orig_genus", "orig_species", "category")
  }
  else if(is.data.frame(splist)){
    xx <- names(splist)
    xx[xx == var_name] <- "orig_name"

    var_name <- c("sorter",
                  xx,
                  "category")
  }
  return(dplyr::select(resultado, dplyr::all_of(var_name)))
}

