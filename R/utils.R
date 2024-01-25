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
