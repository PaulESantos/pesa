#' Git Commit
#'
#' @param msg message
#' @param dir directory name
#'
#' @return a list of commited files
#' @export
#'
gitcommit <- function(msg = "commit from Rstudio",
                      dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}
