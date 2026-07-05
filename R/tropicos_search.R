#' Search botanical names in the Tropicos database
#'
#' @description
#' This function queries the Tropicos API for a vector of scientific species names
#' and returns matching records from the Tropicos database as a tidy tibble.
#'
#' @param species A character vector of species names to search for.
#' @param api_key A character string specifying the Tropicos API key. If `NULL` (default),
#'   the function looks for the `TROPICOS_KEY` or `TROPICOS_API_KEY` environment variables.
#'
#' @return A tibble containing the search results returned by the Tropicos API,
#'   including name ID, scientific name, authorship, and family. If no records are found,
#'   or queries fail, it returns a tibble with 0 rows or missing fields.
#'
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' species_list <- c("Guatteria alutacea", "Oreopanax divulsum")
#' results <- tropicos_search(species_list, api_key = "your_api_key")
#' }
#'
#' @export
tropicos_search <- function(species, api_key = NULL) {
  # Obtener la clave de API
  key <- api_key
  if (is.null(key)) {
    key <- Sys.getenv("TROPICOS_KEY")
    if (key == "") {
      key <- Sys.getenv("TROPICOS_API_KEY")
    }
  }

  if (is.null(key) || key == "") {
    stop(
      "Por favor, suministra una clave de API de Tropicos en el argumento 'api_key' o configura la variable de entorno 'TROPICOS_KEY'."
    )
  }

  url_base <- "https://services.tropicos.org/Name/Search"

  query_single <- function(sp) {
    params <- list(
      name = sp,
      apikey = key,
      format = "json"
    )

    response <- tryCatch(
      {
        httr::GET(url_base, query = params, httr::timeout(15))
      },
      error = function(e) {
        warning(
          "Fallo en la conexion para la especie: ",
          sp,
          ". Detalle: ",
          e$message
        )
        return(NULL)
      }
    )

    if (is.null(response)) {
      return(NULL)
    }

    if (httr::status_code(response) != 200) {
      warning(
        "La solicitud para '",
        sp,
        "' fallo con codigo HTTP: ",
        httr::status_code(response)
      )
      return(NULL)
    }

    txt_content <- httr::content(response, as = "text", encoding = "UTF-8")

    # Manejar caso de que la respuesta sea vacia o contenga errores
    parsed <- tryCatch(
      {
        jsonlite::fromJSON(txt_content)
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Si la API retorna un objeto que indica error
    if (is.null(parsed) || (is.list(parsed) && !is.null(parsed$Error))) {
      return(NULL)
    }

    # Si retorna un dataframe vacio
    if (is.data.frame(parsed) && nrow(parsed) == 0) {
      return(NULL)
    }

    # Convertir a tibble y añadir columna con el termino buscado
    df <- tibble::as_tibble(parsed)
    df$searched_name <- sp
    return(df)
  }

  results_list <- purrr::map(species, query_single)

  # Remover elementos NULL de la lista
  results_list <- results_list[!sapply(results_list, is.null)]

  if (length(results_list) == 0) {
    return(tibble::tibble())
  }

  # Combinar todos los dataframes
  combined_df <- dplyr::bind_rows(results_list)
  return(combined_df)
}
