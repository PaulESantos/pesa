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
read_plus <- function(files_path, format = "csv", ...){
  read_txt_csv <- function(flnm) {
    data.table::fread(flnm, ...) |>
      dplyr::mutate(filename = flnm)
  }

  read_excels <- function(flnm) {
    readxl::read_excel(flnm, ...) |>
      dplyr::mutate(filename = flnm)
  }
  if (format %in% c("xlsx", "xls")){
    files <- list.files(path = files_path,
                        pattern = paste0("*.", format),
                        full.names = TRUE)
    if (length(files) == 0){
      cat("Check the extension of your files. No " %+% crayon::red(format) %+% " files were found")
      #message(paste0("Check the extension of your files. No",
      #              format, "files were found"))
    }
    else{
      files |>
        purrr::map_df(~read_excels(.))
    }

  }
  else if (format %in% c("txt", "csv")){
    files <- list.files(path = files_path,
                        pattern = paste0("*.", format),
                        full.names = TRUE)
    if (length(files) == 0){
      cat("Check the extension of your files. No " %+% crayon::green(format) %+% " files were found")
      #message(paste0("Check the extension of your files. No ",
      #               format, " files were found"))
    }
    else{
      files |>
        purrr::map_df(~read_txt_csv(.))
    }
  }
  else {
    cat(crayon::green("Format not supported"))
  }

}
