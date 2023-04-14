#' Create a README.Rmd file in a specified path
#'
#' This function creates a new folder if it does not exist and creates a README.Rmd file with default content in the specified path.
#'
#' @param path A character string specifying the path where the README.Rmd file will be created.
#' @return None
#'
#' @examples
#' #create_readme_rmd("~/my_project")
#' @export
create_readme_rmd <- function(path) {
  # crea una nueva carpeta si no existe
  if (!file.exists(path)) {
    dir.create(path)
  }

  # define el path completo del archivo README.Rmd
  readme_path <- file.path(path, "README.Rmd")

  # verifica si el archivo ya existe
  if (!file.exists(readme_path)) {
    # crea el archivo README.Rmd con el contenido predeterminado
    writeLines(c(
      "---",
      "output: github_document",
      "---",
      "",
      "<!-- README.md is generated from README.Rmd. Please edit that file -->",
      "",
      "```{r, include = FALSE}",
      "knitr::opts_chunk$set(",
      "  collapse = TRUE,",
      "  comment = \"#>\"",
      ")",
      "```",
      "",
      "",
      "# Project Title",
      "",
      "<!-- badges: start -->",
      "<!-- badges: end -->",
      "",
      "The goal of ...... is to ...",
      "",
      "What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:",
      "",
      "```{r cars}",
      "summary(cars)",
      "```",
      "",
      "You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.",
      "",
      "You can also embed plots, for example:",
      "",
      "```{r pressure, echo = FALSE}",
      "plot(pressure)",
      "```",
      "",
      "In that case, don't forget to commit and push the resulting figure files, so they display on GitHub."
    ), readme_path)

    message("Archivo README.Rmd creado en ", path)

    # abre automáticamente el archivo README.Rmd
    utils::file.edit(readme_path)
  } else {
    message("El archivo README.Rmd ya existe en ", path)

    # abre automáticamente el archivo README.Rmd
    utils::file.edit(readme_path)
  }
}
