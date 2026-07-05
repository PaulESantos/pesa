#' read_plus
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param files_path Path where files are stored.
#' @param format Define the format extension in which files are saved. Could be: "xlsx", "xls", "txt" or "csv".
#' @param ... Further arguments to be passed to read_plus.
#'
#' @return A data frame (tibble) containing a representation of the data in the file.
#' @export
#'
read_plus <- function(files_path, format = "csv", ...) {
  read_txt_csv <- function(flnm) {
    data.table::fread(flnm, ...) |>
      dplyr::mutate(filename = flnm)
  }

  read_excels <- function(flnm) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "El paquete 'readxl' es necesario para leer archivos de Excel. Por favor, instalalo."
      )
    }
    readxl::read_excel(flnm, ...) |>
      dplyr::mutate(filename = flnm)
  }

  cat_msg <- function(text_pre, word, text_post, color_fun = "red") {
    if (requireNamespace("crayon", quietly = TRUE)) {
      color_fn <- if (color_fun == "red") crayon::red else crayon::green
      cat(paste0(text_pre, color_fn(word), text_post))
    } else {
      cat(paste0(text_pre, word, text_post))
    }
  }

  if (format %in% c("xlsx", "xls")) {
    files <- list.files(
      path = files_path,
      pattern = paste0("\\.", format, "$"),
      ignore.case = TRUE,
      full.names = TRUE
    )
    if (length(files) == 0) {
      cat_msg(
        "Check the extension of your files. No ",
        format,
        " files were found",
        "red"
      )
    } else {
      files |>
        purrr::map_df(~ read_excels(.))
    }
  } else if (format %in% c("txt", "csv")) {
    files <- list.files(
      path = files_path,
      pattern = paste0("\\.", format, "$"),
      ignore.case = TRUE,
      full.names = TRUE
    )
    if (length(files) == 0) {
      cat_msg(
        "Check the extension of your files. No ",
        format,
        " files were found",
        "green"
      )
    } else {
      files |>
        purrr::map_df(~ read_txt_csv(.))
    }
  } else {
    if (requireNamespace("crayon", quietly = TRUE)) {
      cat(crayon::green("Format not supported"))
    } else {
      cat("Format not supported")
    }
  }
}
