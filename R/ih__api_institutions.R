#' @title Get institutions from The New York Botanical Index Herbariorum API
#'
#' @description
#' This function allows users to get a dataframe of institutions from The New York Botanical Index Herbariorum API.
#'
#' @param country character string with the name of the country.
#'
#' @return A dataframe with the institutions for the given country.
#'
#' @examples
#' ih_api_institutions("Brazil")
#'
#' @export
#'
ih_api_institutions <- function(country){
  #base url to the api of The New York Botanical Index Herbariorum API
  url_base <- "http://sweetgum.nybg.org/science/api/v1/institutions/search?country="
  #set the country name to made a request
  url <- paste0(url_base, tolower(country))
  #get the request
  df <- jsonlite::fromJSON(url)$data
  #return_df
  return(df)
}
