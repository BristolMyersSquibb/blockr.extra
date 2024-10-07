test_that("filter-expr-block", {
  data <- datasets::iris
  block <- new_filter_expr_block(value = c("Species == 'virginica'"))
  block <- initialize_block(block, data)

  expect_s3_class(block, "filter_expr_block")
  expect_type(block, "list")

  expect_true(is_initialized(block))

  res <- evaluate_block(block, data)
  expect_identical(unique(as.character(res$Species)), "virginica")

  res_ui <- generate_ui(new_filter_expr_block(data = datasets::iris), id = "test")
  expect_s3_class(res_ui, "shiny.tag")
})

test_that("filter-expr-block module_server handles input correctly", {

  # wrap generate_server
  # id as first argument, so we can test via shiny::testSever
  module_server_test <- function(id, x, in_dat, is_prev_valid, ...) {
    generate_server(x = x, in_dat = in_dat, id = id, is_prev_valid = is_prev_valid)
  }

  shiny::testServer(
    module_server_test, {
      session$setInputs(`value-i_add` = 1)  # click something to initialize

      # Simulate input to the ACE Editor fields
      session$setInputs(`value-pl_1_val` = "Species == 'virginica'")

      # Assuming there's a mechanism to trigger an action (e.g., a submit button)
      # You need to simulate that action here
      session$setInputs(`value-i_submit` = 1)

      # Test if the reactive value is updated correctly
      # This will depend on how your module processes these inputs
      expect_identical(unique(as.character(out_dat()$Species)), "virginica")
    },
    args = list(
      id = "test",
      x = new_filter_expr_block(data = datasets::iris),
      in_dat = reactive(datasets::iris),
      is_prev_valid = shiny::reactive(TRUE)
    )
  )
})
