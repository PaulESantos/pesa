# Instalar y cargar la librería httr
#install.packages("httr")
library(httr)
library(jsonlite)

species <- c("Saurauia loeseneriana", "Macropharynx gigantea")
data_json <- jsonlite::toJSON(unname(species))
data_json
tropicos <- function(variables) {
  # URL del API
  url <- "https://services.tropicos.org/Name/Search"

  # Parámetros de la solicitud
  params <- list(
    nameid = "",
    name = data_json,
    apikey = tropi_key,
    format = "json"
  )

  # Realizar la solicitud
  response <- GET(url, query = params)

  # Verificar el código de estado de la respuesta
  if (status_code(response) == 200) {
    # La solicitud fue exitosa
    # Convertir la respuesta a un data.frame
    df <- fromJSON(content(response, "text"))
  } else {
    # La solicitud falló
    print(paste("La solicitud falló con el código de estado", status_code(response)))
  }
  df
}
?status_code()


df |>  janitor::clean_names()
taxize::tp_search("Guatteria alutacea", key = tropi_key)




# -------------------------------------------------------------------------

library(httr)
library(jsonlite)

# URL del API
url <- "https://services.tropicos.org/Name/Search"

# Lista de nombres para buscar
nombres <- c("Guatteria alutacea", "Oreopanax divulsum")

# Lista para almacenar los resultados de búsqueda
resultados <- list()

# Realizar la búsqueda para cada nombre
for (nombre in nombres) {
  # Parámetros de la solicitud
  params <- list(
    #  nameid = "",
    name = nombre,
    apikey = tropi_key,
    format = "json"
  )

  # Realizar la solicitud
  response <- GET(url, query = params)

  # Verificar el código de estado de la respuesta
  if (status_code(response) == 200) {
    # La solicitud fue exitosa
    # Convertir la respuesta a un data.frame y almacenar en la lista de resultados
    resultados[[nombre]] <- fromJSON(content(response, "text"))
  } else {
    # La solicitud falló
    print(paste("La solicitud falló con el código de estado",
                status_code(response)))
  }
}
resultados |>  class()
length(resultados)
rbind(resultados)

# Combinar todos los resultados en un solo data.frame
do.call("rbind", resultados)
df <- bind_rows(resultados)

dim(df)
df
resultados[1]
resultados[2]
do.call(rbind, lapply(resultados, data.frame, stringsAsFactors=FALSE))
