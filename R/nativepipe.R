#' Insert native pipe
#' @name `%>%`
#' @rdname nativepipe
#' @export
insert_n_pipe <- function() {
  rstudioapi::insertText(" |> ")
}
