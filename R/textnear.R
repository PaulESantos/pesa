#' Fuzzy Join Between Two Data Frames Based on String Distance
#'
#' @description
#' Performs fuzzy matching between two data frames using approximate string matching.
#' Similar to dplyr joins but allows for inexact matches based on string distance metrics.
#' Supports multiple join types (inner, left, right, full, semi, anti) and various
#' string distance algorithms.
#'
#' @param x A data frame or data.table. The left table in the join operation.
#' @param y A data frame or data.table. The right table in the join operation.
#' @param by.x Character string. Name of the column in `x` to use for matching.
#' @param by.y Character string. Name of the column in `y` to use for matching.
#' @param type Character string specifying the join type. One of:
#'   \itemize{
#'     \item `"inner"`: Returns only matching rows from both tables (default)
#'     \item `"left"`: Returns all rows from x, matching rows from y
#'     \item `"right"`: Returns all rows from y, matching rows from x
#'     \item `"full"`: Returns all rows from both tables
#'     \item `"semi"`: Returns rows from x that have a match in y (no columns from y)
#'     \item `"anti"`: Returns rows from x that have NO match in y
#'   }
#' @param method Character string. Distance metric for string matching. Options include:
#'   `"osa"` (Optimal String Alignment, default), `"lv"` (Levenshtein),
#'   `"dl"` (Damerau-Levenshtein), `"hamming"`, `"lcs"`, `"qgram"`, `"cosine"`,
#'   `"jaccard"`, `"jw"` (Jaro-Winkler), `"soundex"`. See `stringdist::stringdist`
#'   for details.
#' @param maxDist Numeric. Maximum allowed distance for a match. Strings with distance
#'   greater than this value will not match. Default is 4.
#' @param weight Numeric vector with names `d`, `i`, `s`, `t`. Weights for deletion,
#'   insertion, substitution, and transposition operations. Default: `c(d=1, i=1, s=1, t=1)`.
#' @param p Numeric. Penalty parameter for Jaro-Winkler distance (0-0.25). Default is 0.
#' @param bt Numeric. Boost threshold for Jaro-Winkler distance. Default is 0.
#' @param q Integer. Size of q-grams for q-gram based distances. Default is 1.
#' @param nomatch Value to return when no match is found. Default is `NA_integer_`.
#' @param matchNA Logical. Should NA values match each other? Default is `TRUE`.
#' @param useBytes Logical. Should matching be done byte-by-byte rather than
#'   character-by-character? Default is `FALSE`.
#' @param nthread Integer. Number of threads to use for parallel processing.
#'   Default uses option `"sd_num_thread"` or 1 if not set.
#' @param suffixes Character vector of length 2. Suffixes to append to duplicate
#'   column names from x and y. Default is `c(".x", ".y")`.
#'
#' @return A data.table with the joined result. Column naming follows data.table
#'   merge conventions with the specified suffixes for duplicate names.
#'
#' @details
#' The function uses `stringdist::amatch()` for approximate string matching and
#' `data.table` for efficient join operations.
#'
#' **Join Logic:**
#' \itemize{
#'   \item For `right` joins, matches are computed from y to x (reverse direction)
#'   \item For other joins, matches are computed from x to y (standard direction)
#'   \item `full` joins combine matching rows plus unmatched rows from both tables
#'   \item `semi` and `anti` joins return early without merging tables
#' }
#'
#' **Performance Note:** Fuzzy matching can be computationally expensive for large
#' datasets. Consider filtering data or using more restrictive `maxDist` values
#' for better performance.
#'
#' @examples
#' \dontrun{
#' # Inner join with default OSA distance
#' df1 <- data.frame(name = c("John Smith", "Jane Doe", "Bob Johnson"))
#' df2 <- data.frame(nombre = c("Jon Smith", "Jane Do", "Robert Johnson"))
#'
#' result <- textnear(df1, df2, by.x = "name", by.y = "nombre", maxDist = 2)
#'
#' # Left join with Jaro-Winkler distance
#' result <- textnear(df1, df2,
#'                    by.x = "name", by.y = "nombre",
#'                    type = "left",
#'                    method = "jw",
#'                    maxDist = 0.1)
#'
#' # Anti join to find unmatched records
#' unmatched <- textnear(df1, df2,
#'                       by.x = "name", by.y = "nombre",
#'                       type = "anti",
#'                       maxDist = 3)
#' }
#'
#' @seealso
#' \code{\link[stringdist]{amatch}} for the underlying matching algorithm,
#' \code{\link[data.table]{merge}} for standard exact joins
#'
#' @importFrom data.table data.table copy setnames
#' @importFrom stringdist amatch
#'
#' @export
textnear <- function(
    x,
    y,
    by.x,
    by.y,
    type          = "inner",
    method        = "osa",
    maxDist       = 4,
    weight        = c(d = 1, i = 1, s = 1, t = 1),
    p             = 0,
    bt            = 0,
    q             = 1,
    nomatch       = NA_integer_,
    matchNA       = TRUE,
    useBytes      = FALSE,
    nthread       = getOption("sd_num_thread", 1L),
    suffixes      = c(".x", ".y")
) {

  # Validaciones básicas
  type <- match.arg(type, c("inner", "left", "right", "full", "semi", "anti"))

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Requiere paquete data.table")
  }
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Requiere paquete stringdist")
  }

  # Convertir a data.table y crear copias para evitar modificar objetos originales
  x <- copy(as.data.table(x))
  y <- copy(as.data.table(y))

  # Crear claves internas únicas (índices de fila) para rastrear coincidencias
  ukx <- ".unique_key_x"
  uky <- ".unique_key_y"
  x[, (ukx) := .I]
  y[, (uky) := .I]

  # ────────────────────────────────────────────────────────────────
  # 1. Right join → coincidencia inversa (de y hacia x)
  # ────────────────────────────────────────────────────────────────
  if (type == "right") {
    # Para right join, buscar coincidencias de y en x
    match_idx <- stringdist::amatch(
      x         = y[[by.y]],
      table     = x[[by.x]],
      method    = method,
      maxDist   = maxDist,
      weight    = weight,
      p         = p,
      bt        = bt,
      q         = q,
      nomatch   = nomatch,
      matchNA   = matchNA,
      useBytes  = useBytes,
      nthread   = nthread
    )
    y[, (ukx) := match_idx]
    result <- y[x, on = ukx, nomatch = 0L]
    is_right_join <- TRUE
  } else {
    # ────────────────────────────────────────────────────────────────
    # Otros joins → coincidencia normal (de x hacia y)
    # ────────────────────────────────────────────────────────────────
    match_idx <- stringdist::amatch(
      x         = x[[by.x]],
      table     = y[[by.y]],
      method    = method,
      maxDist   = maxDist,
      weight    = weight,
      p         = p,
      bt        = bt,
      q         = q,
      nomatch   = nomatch,
      matchNA   = matchNA,
      useBytes  = useBytes,
      nthread   = nthread
    )
    x[, (uky) := match_idx]

    # Casos especiales: semi y anti joins retornan solo filas de x
    if (type == "semi") {
      return(x[!is.na(get(uky)), !c(ukx, uky), with = FALSE])
    }
    if (type == "anti") {
      return(x[is.na(get(uky)), !c(ukx, uky), with = FALSE])
    }

    # Left / inner / full: realizar join base
    result <- x[y, on = uky, nomatch = if (type == "inner") 0L else NA]
    is_right_join <- FALSE
  }

  # ────────────────────────────────────────────────────────────────
  # Full join: agregar filas no coincidentes de la tabla derecha
  # ────────────────────────────────────────────────────────────────
  if (type == "full" && !is_right_join) {
    # Identificar filas de y que no tienen coincidencia
    matched_uky <- unique(result[!is.na(get(uky))][[uky]])
    unmatched_y <- y[!get(uky) %chin% matched_uky]

    # Crear estructura vacía compatible con result
    full_cols <- names(result)
    unmatched_dt <- data.table::data.table(
      matrix(NA, nrow = nrow(unmatched_y), ncol = length(full_cols))
    )
    setnames(unmatched_dt, full_cols)

    # Copiar columnas de y con prefijo "i." (convención de data.table merge)
    y_cols <- setdiff(names(y), by.y)
    for (col in y_cols) {
      if (col %in% names(unmatched_dt)) {
        unmatched_dt[[col]] <- unmatched_y[[col]]
      } else {
        unmatched_dt[[paste0("i.", col)]] <- unmatched_y[[col]]
      }
    }

    # Combinar filas coincidentes con no coincidentes
    result <- rbind(result, unmatched_dt, use.names = TRUE, fill = TRUE)
  }

  # ────────────────────────────────────────────────────────────────
  # Renombrar columnas duplicadas con sufijos
  # ────────────────────────────────────────────────────────────────
  all_cols <- names(result)
  i_cols <- grep("^i\\.", all_cols, value = TRUE)

  if (length(i_cols) > 0) {
    overlaps <- sub("^i\\.", "", i_cols)

    for (i in seq_along(i_cols)) {
      orig_col <- overlaps[i]
      i_col    <- i_cols[i]

      # Determinar sufijos según el tipo de join
      suffix_y <- if (is_right_join) suffixes[1] else suffixes[2]
      suffix_x <- if (is_right_join) suffixes[2] else suffixes[1]

      # Renombrar columna con prefijo "i." (de tabla y)
      setnames(result, i_col, paste0(orig_col, suffix_y))

      # Renombrar columna original (de tabla x) si existe y tiene sufijo
      if (orig_col %in% names(result) && suffix_x != "") {
        setnames(result, orig_col, paste0(orig_col, suffix_x))
      }
    }
  }

  # Eliminar claves internas temporales
  result[, c(ukx, uky) := NULL]

  return(result[])
}
