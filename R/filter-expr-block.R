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
    # value = new_keyvalue_field(value = value),
    value = new_keyvalue_field(value = value, submit = TRUE, multiple = TRUE, key = "none"),
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


# A simple, one column layout
#
# we need the fields, not the rendered fields, that's why I can't use the layout
# methods

#' @importFrom shiny p tags div
ui_fields.filter_expr_block <- function(x, ns, inputs_hidden, ...) {

  html_card <- function(field, field_name) {
    div(
      div(
        tags$h5(attr(field, "title")),
        div(
          class = c("text-muted small mb-2"),
          attr(field, "descr")
        )
      ),
      tags$div(
        class = "w-100",
        ui_input(field, id = ns(field_name), name = NULL)
      )
    )
  }

  cards <- Map(
    html_card,
    field = x,
    field_name = names(x)
  )

  div(
    class = sprintf("block-inputs %s", inputs_hidden),
    div(cards, class = "mt-3")
  )

}
