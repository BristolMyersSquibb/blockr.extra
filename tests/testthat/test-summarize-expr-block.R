test_that("summarize-expr-block", {
  data <- datasets::iris

  block <- new_summarize_expr_block(
    group_by = "Species",
    new_cols = c(
      Mean.Sepal.Width = "mean(Sepal.Width)"
    )
  )

  block <- initialize_block(block, data)

  expect_true(is_initialized(block))
  expect_s3_class(block, "summarize_expr_block")
  expect_type(block, "list")

  res <- evaluate_block(block, data)
  expect_identical(nrow(res), 3L)

})
