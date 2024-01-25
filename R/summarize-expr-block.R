# pkgload::load_all(".");
# stack <- new_stack(data_block, summarize_expr_block);
# serve_stack(stack)

#' @importFrom admiral derive_param_computed
#' @importFrom rlang exprs
#' @import blockr
new_summarize_expr_block <- function(data,
                                     new_cols = NULL,
                                     group_by = NULL,
                                     ...) {

  # 0. helper functions
  handle_error <- function(e) {
    warning(e)
    expression()
  }

  parse_one <- function(x) {
    tryCatch(
      parse(text = x),
      error = handle_error
    )
  }

  parse_multiple <- function(x) {
    ans <- try({
      res <- lapply(x, parse_one)
      if (length(res)) {
        res <- do.call(c, res)
      }
    })
    if (inherits(ans, "try-error")) {
      return(expression())
    }
    res
  }

  parse_cols <- function(new_cols) {
    parse_multiple(new_cols)
  }

  parse_grouping <- function(group_by) {
    res <- parse_multiple(group_by)
    names(res) <- NULL
    res
  }

  # 1. main expression
  main_expr <- function(new_cols = NULL, group_by = NULL) {

    col_expr <- parse_cols(new_cols)
    grp_expr <- parse_grouping(group_by)

    bquote(
      dplyr::summarize(..(col_expr), .by = c(..(grp_expr))),
      list(col_expr = col_expr, grp_expr = grp_expr),
      splice = TRUE
    )
  }

  # 2. functions for derived inputs
  data_cols <- function(data) {
    colnames(data)
  }

  # 3. fields
  fields <- list(
    new_cols = new_keyvalue_field(
      value = new_cols,
      submit = TRUE,
      title = "Summarise each group down to one row",
      descr = "Name-value pairs of summary functions. The name will be the name
               of the variable in the result."
    ),
    group_by = new_select_field(
      value = group_by,
      choices = data_cols,
      multiple = TRUE,
      title = "Grouping variables",
      descr = "Selection of columns to group by for just this
              operation, functioning as an alternative to group_by()"
    ),
    expression = new_hidden_field(main_expr)
  )

  # 4. block creation
  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ...,
    class = c("summarize_expr_block", "transform_block")
  )
}

#' Summarize By Block
#'
#' @inheritParams blockr::new_block
#'
#' @export
summarize_expr_block <- function(data, ...) {
  initialize_block(new_summarize_expr_block(data, ...), data)
}


#' @export
ui_fields.summarize_expr_block <- function(x, ns, inputs_hidden, ...) {
  ui_fields_one_column(x = x, ns = ns, inputs_hidden = inputs_hidden)
}
