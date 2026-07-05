test_that("simple_cap works correctly", {
  expect_equal(
    simple_cap("Miconia    secundifolia  subsp.  Secundifolia"),
    "Miconia secundifolia subsp. secundifolia"
  )
  expect_equal(
    simple_cap(c("apple pie", " BANANA  SHAKE ")),
    c("Apple pie", "Banana shake")
  )
})

test_that("str_delete_before is vectorized and works correctly", {
  # Test de un solo elemento
  expect_equal(
    str_delete_before("abc_def_ghi", "_", pos = 1),
    "ghi"
  )
  expect_equal(
    str_delete_before("abc_def_ghi", "_", pos = 2),
    "def_ghi"
  )

  # Test de vectorización (antes fallaba por usar pos_pattern[[1]])
  vec <- c("abc_def_ghi", "x_y_z_w")
  expect_equal(
    str_delete_before(vec, "_", pos = 1),
    c("ghi", "w")
  )
  expect_equal(
    str_delete_before(vec, "_", pos = 2),
    c("def_ghi", "z_w")
  )
})

test_that("str_delete_after is vectorized and works correctly", {
  # Test de un solo elemento
  expect_equal(
    str_delete_after("abc_def_ghi", "_", pos = 1),
    "abc_def"
  )
  expect_equal(
    str_delete_after("abc_def_ghi", "_", pos = 2),
    "abc"
  )

  # Test de vectorización
  vec <- c("abc_def_ghi", "x_y_z_w")
  expect_equal(
    str_delete_after(vec, "_", pos = 1),
    c("abc_def", "x_y_z")
  )
  expect_equal(
    str_delete_after(vec, "_", pos = 2),
    c("abc", "x_y")
  )
})
