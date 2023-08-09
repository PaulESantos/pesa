#' Write Simple Features to an ESRI Shapefile
#'
#' This function writes a Simple Features (sf) object to an ESRI Shapefile.
#' It drops the Z and M dimensions from the object before writing.
#'
#' @param sf A Simple Features (sf) object to be written to the Shapefile.
#' @param folder The folder where the Shapefile will be created.
#' @param filename The desired filename for the Shapefile (without the .shp extension).
#'
#' @return None. The function writes the SF object to a Shapefile.
#'
#' @examples
#' \dontrun{
#' # Write an SF object to a Shapefile
#' shape_write(my_sf_object, "output_folder", "output_filename")
#' }
#'
#' @export
shape_write <- function(sf, folder, filename) {
  sf::st_zm(sf, drop = TRUE, what = 'ZM') |>
    sf::st_write(dsn = folder,     # Folder
                 layer = filename,  # Filename
                 driver = "ESRI Shapefile",
                 append = FALSE)
}
