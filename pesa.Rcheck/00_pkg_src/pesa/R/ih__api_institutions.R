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
#' \dontrun{
#' ih_api_institutions("Brazil")
#' }
#'
#' @export
#'
ih_api_institutions <- function(country) {
  # Base URL to the API of The New York Botanical Index Herbariorum API
  url_base <- "http://sweetgum.nybg.org/science/api/v1/institutions/search"

  params <- list(country = tolower(country))

  # Intentar realizar la conexion con control de errores
  response <- tryCatch(
    {
      httr::GET(url_base, query = params, httr::timeout(15))
    },
    error = function(e) {
      stop(
        "No se pudo establecer conexion con la API de Index Herbariorum. Verifica tu conexion a internet. Detalles: ",
        e$message
      )
    }
  )

  # Validar el codigo de respuesta HTTP
  if (httr::status_code(response) != 200) {
    stop(
      "La API de Index Herbariorum retorno un error HTTP con codigo: ",
      httr::status_code(response)
    )
  }

  # Parsear el contenido JSON de la respuesta
  content_txt <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(content_txt)

  df <- parsed_data$data
  return(df)
}
