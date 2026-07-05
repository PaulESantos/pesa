#' Agregar repeticiones basadas en expresiones regulares
#'
#' Esta función busca valores que coinciden con un patrón de expresión regular en una columna de un dataframe
#' y agrega repeticiones de esos valores en una nueva columna.
#'
#' @param data El dataframe original.
#' @param column_name El nombre de la columna donde se realizará la búsqueda.
#' @param regex_pattern El patrón de expresión regular a buscar en la columna especificada.
#' @param n_repetitions El número de repeticiones que se agregarán después de cada coincidencia.
#' @param new_column_name El nombre de la nueva columna donde se agregarán las repeticiones.
#' @return El dataframe resultante con la nueva columna agregada.
#' @examples
#' df <-  tibble::tibble(
#'   var1 = c("a_1", "d", "d", "a_2", "d", "f", "a_3", "g", "u")
#' )
#' df_result <- add_repetitions_regex(df, "var1", "^(a|f)", 2, "new_var")
#' print(df_result)
#' @importFrom rlang `:=`
#' @export
add_repetitions_regex <- function(
  data,
  column_name,
  regex_pattern,
  n_repetitions,
  new_column_name
) {
  # Copiar el dataframe original
  df <- data

  col_vals <- df[[column_name]]
  new_vals <- rep(NA_character_, length(col_vals))

  matches <- which(grepl(regex_pattern, col_vals))

  for (i in matches) {
    if (n_repetitions > 0) {
      target_indices <- (i + 1):min(i + n_repetitions, length(col_vals))
      new_vals[target_indices] <- col_vals[i]
    }
  }

  # Agregar la nueva variable con el vector asignado directamente
  df[[new_column_name]] <- new_vals

  return(df)
}
