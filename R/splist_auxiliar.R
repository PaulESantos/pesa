#' @keywords internal
.names_standardize <- function(splist) {
  # Convertir todo a mayúsculas
  fixed1 <- toupper(splist)

  # Eliminar 'CF.' y 'AFF.'
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)

  # Eliminar espacios en blanco al inicio y al final
  fixed4 <- trimws(fixed3)

  # Cambiar guiones bajos por espacios
  fixed5 <- gsub("_", " ", fixed4)

  # Estandarizar 'VAR', 'F.', 'SUBSP.'
  fixed6 <- gsub(" VAR ", " VAR. ", fixed5)
  fixed7 <- gsub(" (F|FO|FO\\.|FORM|FORM\\.|FORMA|FORMA\\.) ", " F. ", fixed6)
  fixed8 <- gsub(" (SSP|SPP|SUBSP|SP|SP\\.|SPP\\.) ", " SUBSP. ", fixed7)

  # Manejar híbridos (eliminar 'X' y '\u00d7')
  fixed9 <- gsub("(^X )|( X$)|( X )|(^\u00d7 )|( \u00d7$)|( \u00d7 )", " ", fixed8)
  hybrids <- fixed8 == fixed9
  if (!all(hybrids)) {
    sp_hybrids <- splist[!hybrids]
    warning(paste("The 'X' sign indicating hybrids have been removed in the",
                  "following names before search:",
                  paste(paste0("'", sp_hybrids, "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }

  # Eliminar múltiples espacios
  fixed10 <- gsub(" +", " ", fixed9)

  # Eliminar símbolos no alfabéticos al inicio
  for(j in 1:100) {
    whichs <- which(grepl("^[^A-Z]", fixed10))
    if(length(whichs) > 0)
      fixed10[whichs] <- gsub("^[^A-Z]", "", fixed10[whichs])
    whichs <- which(grepl("^[^A-Z]", fixed10))
    if(length(whichs) == 0) break
  }

  return(fixed10)
}
#------------------------------------------------
#' @keywords internal
# Function wrap of .classify_algo for multiple species
.splist_classify <- function(x) {

  x <- .names_standardize(x)

  ##################
  infrasp <- c("subsp.", "ssp.", "var.", "subvar.",
               "forma", "f.", "subf.")
  Infrasp_cat <- toupper(infrasp)
  # Regular expression to make sure, infra code is between names
  Infrasp_cat_reg <- paste("[[:alpha:]]",
                           gsub("\\.",
                                "\\\\.",
                                Infrasp_cat),
                           "[[:alpha:]]")
  Infrasp_cat_reg |>  length()
  # Split names
  x_split <- strsplit(x, " ")

  # Aply the algorithm
  result <- lapply(x_split,
                   .classify_algo,
                   Infrasp_cat_reg)
  # Combine result list into a matrix
  result <- do.call(rbind, result)
  result <- cbind(x, result)
  # Combine categories and remove
  result[, 5] <- paste0(result[, 5], result[, 6])
  result[, 9] <- paste0(result[, 9], result[, 10])
  result <- result[, -c(6, 10), drop = FALSE]

  # Give the colnames of the matrix
  colnames(result) <- c(
    "orig_name",
    "orig_genus",
    "orig_species",
    "author",
    "subspecies",
    "variety",
    "subvariety",
    "forma",
    "subforma"
  )
  result
  return(result)
}

#------------------------------------------------
# The algorithm for one name
.classify_algo <- function(x_split_i,
                           Infrasp_cat_reg) {

  # Base output
  output <- character(10)

  # Count the number of names
  n <- length(x_split_i)

  # Genus and epithet
  output[1:2] <- x_split_i[1:2]


  # Check for infrataxa
  if (n > 2) {
    # Connect previous and next name to check for infras
    x_split_i_paste <- x_split_i
    x_split_i_paste[2:n] <- paste(substr(x_split_i[1:(n - 1)], 1, 1),
                                  x_split_i[2:n],
                                  substr(x_split_i[3:n],1 , 1))

    infra_check <- sapply(as.list(Infrasp_cat_reg),
                          function(x, y) {
                            regexpr(x, y) == 1
                          },
                          x_split_i_paste)
    infra_id <- rowSums(infra_check) > 0



    # if there is none get only the author name
    if (!any(infra_id)) {
      output[3] <- paste(x_split_i[3:n],
                         collapse = " ")
    } else {
      # If it has infra categories, get them

      n_infra <- sum(infra_id) # Number of infra categories
      pos <- which(infra_id)
      for (i in 1:n_infra) {
        # do it for all infra names
        # Get the position of the infra
        pos_1 <- pos[i] + 1
        pos_out <- which(infra_check[pos[i], ]) + 3
        output[pos_out] <- x_split_i[pos_1]
      }
      if (n > pos_1) {
        # get the author
        output[3] <- paste(x_split_i[(pos_1 + 1):n],
                           collapse = " ")
      }
      if (pos[1] > 3) { # Author names before infras
        output[3] <- paste(x_split_i[3:(pos[1] - 1)],
                           collapse = " ")
      }
    }
  }
  return(output)
}


#' @keywords internal
.check_binomial <- function(splist_class) {

  splist_class$binomial <- ifelse(
    !is.na(splist_class$orig_genus) & !is.na(splist_class$orig_species),
    "binomial",
    "non binomial"
  )

    missing_bino <- splist_class[splist_class$binomial == "non binomial",]


  if (length(missing_bino) > 0) {
    message(paste0("The species list (splist) should only include binomial names.",
                   " The following names were submitted at the genus level: ",
                   paste(paste0("'", missing_bino$orig_name, "'"),
                         collapse = ", ")))

  }

  return(splist_class)
}


# ---------------------------------------------------------------
#' Transform classified species data
#'
#' Esta función transforma una tabla resultante de la clasificación taxonómica de nombres científicos,
#' añadiendo columnas auxiliares como `orig_infraspecies`, `infra_rank` y `rank`, útiles para análisis posteriores.
#'
#' @param df Data frame generado por la función `.splist_classify()`, el cual contiene las columnas
#' `orig_genus`, `orig_species`, `subspecies`, `variety`, `subvariety`, `forma`, `subforma`.
#'
#' @return Un data frame con columnas adicionales:
#' \describe{
#'   \item{sorter}{Índice para mantener el orden original.}
#'   \item{orig_infraspecies}{Nombre de la infraspecie, si aplica.}
#'   \item{infra_rank}{Categoría taxonómica de la infraspecie (ej. SUBSP., VAR., etc.).}
#'   \item{rank}{Nivel taxonómico estimado: 1 = género, 2 = especie, 3 = infraspecie.}
#' }
#'
#' @keywords internal
#' @noRd
.transform_split_classify <- function(df) {
  # Convertir a data frame
  df <- as.data.frame(df)
  df$sorter <- 1:nrow(df)

  # Crear las nuevas columnas infraspecie e infra_rank
  df$orig_infraspecies <- with(df, ifelse(subspecies != "", subspecies,
                                          ifelse(variety != "", variety,
                                                 ifelse(subvariety != "", subvariety,
                                                        ifelse(forma != "", forma,
                                                               ifelse(subforma != "", subforma, NA_character_))))))

  df$infra_rank <- with(df, ifelse(subspecies != "", "SUBSP.",
                                   ifelse(variety != "", "VAR.",
                                          ifelse(subvariety != "", "SUBVAR.",
                                                 ifelse(forma != "", "F.",
                                                        ifelse(subforma != "", "SUBF.", NA_character_))))))

  # Añadir la columna rank
  df$rank <- ifelse(!is.na(df$orig_genus) & !is.na(df$orig_species) & is.na(df$orig_infraspecies), 2,
                    ifelse(!is.na(df$orig_genus) & !is.na(df$orig_species) & !is.na(df$orig_infraspecies), 3,
                           ifelse(is.na(df$orig_species) & is.na(df$orig_infraspecies), 1, NA)))

  # Reordenar las columnas para que infraspecie e infra_rank estén antes de Subspecies
  column_order <- c( "sorter","orig_name",
                     "orig_genus",
                     "orig_species",
                     "author",
                     "orig_infraspecies",
                     "infra_rank",
                     "rank")#,
  # "Subspecies", "Variety", "Subvariety",
  #"Forma", "Subforma")

  df <- df[, column_order]

  return(df)
}


#' Separar nombres científicos en columnas de género y especie
#'
#' Esta función toma un data frame y una columna que contiene nombres científicos en formato "Género especie",
#' y crea dos nuevas columnas: `orig_genus` y `orig_species`. Además, la columna original es renombrada como `orig_name`.
#'
#' @param df Un data frame que contiene una columna con nombres científicos.
#' @param col_name Nombre de la columna (como cadena de texto) que contiene los nombres científicos.
#'
#' @return El mismo data frame con la columna original renombrada a `orig_name`,
#'         y dos nuevas columnas: `orig_genus` y `orig_species`.
#'
#' @keywords internal
.splist_format_df <- function(df, col_name) {
  # Renombrar la columna a 'orig_name'
  names(df)[names(df) == col_name] <- "orig_name"

  # Separar el nombre científico por espacio
  nombre_separado <- strsplit(as.character(df$orig_name), " ")

  # Crear nuevas columnas
  df$orig_genus <- sapply(nombre_separado, function(x) x[1])
  df$orig_species <- sapply(nombre_separado, function(x) ifelse(length(x) > 1, x[2], NA))

  return(df)
}


#' Obtener la categoría de la Lista Roja de la UICN para una especie
#'
#' Esta función consulta la API de la UICN para recuperar la categoría de conservación
#' de una especie, utilizando su género y epíteto específico. Devuelve un tibble con la
#' descripción de la categoría y su código correspondiente.
#'
#' @param genus Cadena de texto que representa el nombre del género de la especie.
#' @param species Cadena de texto que representa el epíteto específico de la especie.
#' @param api_key Objeto de autenticación.
#'
#' @return Un `tibble` con dos columnas:
#' \describe{
#'   \item{category}{Descripción de la categoría de conservación (por ejemplo, "Endangered").}
#'   \item{code}{Código abreviado de la categoría (por ejemplo, "EN").}
#' }
#' Si no se encuentra información o ocurre un error, ambas columnas serán `NA`.
#'
#'
#' @importFrom dplyr select slice pull
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @keywords internal
get_iucn_category <- function(genus, species, api_key) {

  if(is.null(api_key) == TRUE){
    api_key_iucn <- Sys.getenv("iucn_redlist_key")
    # 4. CONECTARSE A LAS APIs
    api <- iucnredlist::init_api(red_list_api_key = api_key_iucn)
  }
  else{
    api <- iucnredlist::init_api(red_list_api_key = api_key)
  }

  # Intentar obtener la evaluación por nombre
  res_1 <- tryCatch({
    iucnredlist::assessments_by_name(api, genus = genus, species = species)
  }, error = function(e) return(NULL))

  if (is.null(res_1) || nrow(res_1) == 0) {
    return(tibble::tibble(category = NA, code = NA))
  }

  assesssment_id <- res_1 |>
    dplyr::select(assessment_id) |>
    dplyr::slice(1) |>
    dplyr::pull()

  assessment_data <- tryCatch({
    iucnredlist::assessment_data(api, assessment_id = assesssment_id)
  }, error = function(e) return(NULL))

  if (is.null(assessment_data)) {
    return(tibble::tibble(category = NA, code = NA))
  }

  assessment_data |>
    purrr::pluck("red_list_category") |>
    (\(x) tibble::tibble(
      category = x$description$en,
      code = x$code
    ))()
}

# ---------------------------------------------------------------
#' @keywords internal
str_to_simple_cap <- function(text) {
  # Convertir todo el texto a minúsculas
  text <- tolower(text)

  # Obtener la primera letra y convertirla a mayúscula
  first_letter <- toupper(substr(text, 1, 1))

  # Obtener el resto del texto desde la segunda letra en adelante
  rest_text <- substr(text, 2, nchar(text))

  # Combinar la primera letra en mayúscula con el resto del texto en minúsculas
  result <- paste0(first_letter, rest_text)

  return(result)
}

