filter_expr_expression <- function(value = c(a = "2.1", b = "4.5")) {
  if (is.null(value)) {
    return(quote(dplyr::filter()))
  }
  stopifnot(inherits(value, "character"))

  parse_one <- function(text) {
    expr <- try(parse(text = text))
    if (inherits(expr, "try-error")) {
      expr <- expression()
    }
    expr
  }

  exprs <- do.call(c, lapply(value, parse_one))

  bquote(
    dplyr::filter(..(exprs)),
    list(exprs = exprs),
    splice = TRUE
  )
}

new_filter_expr_block <- function(data, value = NULL, ...) {
  fields <- list(
    value = new_keyvalue_field(
      value = value,
      submit = TRUE,
      multiple = TRUE,
      key = "none",
      title = "Keep rows that match a condition",
      descr = "The filter() function is used to subset a data frame, retaining
              all rows that satisfy your conditions. To be retained, the row
              must produce a value of TRUE for all conditions."

    ),
    expression = new_hidden_field(filter_expr_expression)
  )

  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ...,
    class = c("filter_expr_block", "transform_block")
  )
}

#' Filter Expression Block
#'
#' @inheritParams blockr::new_block
#'
#' @export
#' @importFrom dplyr filter
filter_expr_block <- function(data, ...) {
  initialize_block(new_filter_expr_block(data, ...), data)
}

#' @export
ui_fields.filter_expr_block <- function(x, ns, inputs_hidden, ...) {
  ui_fields_one_column(x = x, ns = ns, inputs_hidden = inputs_hidden)
}
