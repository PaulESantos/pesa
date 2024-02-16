#' Divide un dataframe en subgrupos y asigna cada subgrupo a un objeto con un
#' nombre limpio
#'
#' Esta función toma un dataframe y una variable por la cual dividirlo en
#' subgrupos. Luego, asigna cada subgrupo a un objeto con un nombre limpio
#' generado a partir de los valores únicos de la variable.
#'
#' @param df El dataframe que se dividirá en subgrupos.
#' @param variable La variable por la cual se dividirá el dataframe en subgrupos.
#' @return No devuelve nada explícitamente, pero asigna los subgrupos a objetos
#'  con nombres limpios en el entorno global.
#' @export
#'
#' @examples
#' \dontrun{
#' # Ejemplo de uso con iris
#' split_and_assign(iris, "Species")
#' }
split_and_assign <- function(df, variable) {
  by_var <- df |>
    dplyr::group_split({{ variable }})

  for (i in seq_along(by_var)) {
    # Extraemos el nombre de la variable del primer tibble de la lista
    var_name <- unique(by_var[[i]][[as.character(substitute(variable))]])[1] |>
      janitor::make_clean_names()

    # Creamos un nuevo objeto con el nombre de la variable y asignamos el tibble correspondiente
    assign(var_name, by_var[[i]], envir = parent.frame())
  }
}
