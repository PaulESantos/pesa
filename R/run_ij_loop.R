#' Automated leaf area analysis
#'
#' @param path Set directory that contains leaf images.
#' @param new_path Set temp directory that contains leaf images.
#' @param output Set output directory that contains tiff images.
#' @param ...
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' #dir <- paste0(here::here(), "/", "inst/images")
#' #temp <-  paste0(here::here(), "/", "inst/temp")
#' #output <-  paste0(here::here(), "/", "inst/output")
#' #data <- run_ij_p(dir, temp, output)
#' #data
run_ij_loop <- function(path, new_path, output, ...){
  #internal
  file_copy <- function(from, to, ...){
    files <- list.files(path = from, full.names = TRUE)
    files_1 <- grep(".jpg|.jpeg", files, value = TRUE)
    file.copy(from = files_1, to = to)
  }
  #internal
  file_copy(from = path, to = new_path)
  area <- try(rleafarea::run_ij(set.directory = new_path,
                     distance.pixel = 300,
                     known.distance = 2.54,
                     log = TRUE,
                     low.size = 0.005,
                     trim.pixel = 60,
                     trim.pixel2 = 150,
                     save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file_copy(dir(new_path,
                full.names = TRUE,
                pattern = "\\.tif"),
            output)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new_path,
                          full.names = TRUE) ))) stop()
  res <- dplyr::tibble(id = names(unlist(area[[2]])),
                       leaf_area = (unlist(area[[2]]))) |>
    dplyr::mutate(id = stringr::str_remove(id,
    paste0(c("\\.jpeg\\.txt\\.Area[0-9]{1,}",
             "\\.jpeg\\.txt\\.Area",
             "\\.jpg\\.txt\\.Area[0-9]{1,}",
             "\\.jpg\\.txt\\.Area"),
           collapse = "|")))

  return(res)
}

