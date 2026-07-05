test_that("add_repetitions_regex works correctly and handles boundaries", {
  df <- tibble::tibble(
    var1 = c("a_1", "d", "d", "a_2", "d", "f", "a_3", "g", "u")
  )

  # Caso de 2 repeticiones
  res <- add_repetitions_regex(df, "var1", "^(a|f)", 2, "new_var")

  # Esperamos que las posiciones después de la coincidencia tomen el valor
  # Coincidencias en:
  # fila 1 ("a_1") -> propaga a 2, 3
  # fila 4 ("a_2") -> propaga a 5, 6 (pero 6 es coincidencia para "f" en la entrada original)
  # fila 6 ("f")   -> propaga a 7, 8
  # fila 7 ("a_3") -> propaga a 8, 9

  expect_equal(
    res$new_var,
    c(NA, "a_1", "a_1", NA, "a_2", "a_2", "f", "a_3", "a_3")
  )

  # Caso con 0 repeticiones
  res_zero <- add_repetitions_regex(df, "var1", "^(a|f)", 0, "new_var")
  expect_true(all(is.na(res_zero$new_var)))
})
