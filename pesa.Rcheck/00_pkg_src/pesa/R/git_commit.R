#' Realiza un commit con todos los cambios en un repositorio Git
#'
#' Esta función verifica si el directorio actual es un repositorio Git y realiza un commit con todos los cambios,
#' incluyendo modificaciones, eliminaciones y creaciones de archivos.
#'
#' @param commit_message El mensaje descriptivo para el commit.
#'
#' @return No devuelve ningún valor. Muestra un mensaje indicando que el commit se realizó con éxito.
#'
#' @examples
#' \dontrun{
#' # Realizar un commit con todos los cambios
#' commit_all_changes("Mensaje del commit")
#' }
#'
#'
#' @export
commit_all_changes <- function(commit_message) {
  # Verificar si el directorio actual es un repositorio Git
  if (!system("git rev-parse --is-inside-work-tree", intern = TRUE) == "true") {
    stop("No se encontro un repositorio Git en el directorio actual.")
  }

  # Agregar todos los cambios a la zona de preparación
  system("git add -A")

  # Realizar el commit con el mensaje proporcionado
  system(paste("git commit -m", shQuote(commit_message)))

  message("Se realizo el commit con exito.")
}
