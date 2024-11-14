#' A simple, one column layout
#'
#' we need the fields, not the rendered fields, that's why we won't use layout
#'
#' @noRd
#' @importFrom shiny p tags div
ui_fields_one_column <- function(x, ns, inputs_hidden) {
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

#' @export
block_input_check.plot_block <- function(x, data, ...) {

  if (inherits(data, "data.frame")) {
    return(invisible(NULL))
  }

  input_failure("Expecting data.frame input.")
}

#' @export
block_output_ptype.plot_block <- function(x, ...) ggplot2::ggplot()

register_blockr_extra_blocks <- function(pkg) {

  if (missing(pkg))
    pkg <- "blockr.extra"

  register_blocks(
    constructor = c(
      new_admiral_dpc_block,
      new_filter_expr_block,
      new_summarize_expr_block,
      new_code_transform_block,
      new_code_plot_block
    ),
    name = c(
      "admiral dpc block",
      "filter expr block",
      "summarize expr block",
      "code transform block",
      "code plot block"
    ),
    description = c(
      "Admiral block",
      "Filter expr block",
      "Summarize expr block",
      "Code transform block",
      "Code plot block"
    ),
    category = c("transform", "transform", "transform", "transform", "plot"),
    package = pkg
  )
}
