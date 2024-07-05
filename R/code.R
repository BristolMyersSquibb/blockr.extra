#' Code field
#' @param value Initial code.
#' @param x Object.
#' @param ... Ignored.
#' @name code_field
#' @export
new_code_field <- function(value = character(), ...) {
  new_field(
    value,
    class = "code_field"
  )
}

#' @rdname code_field
#' @export
validate_field.code_field <- function(x) {
  x
}

#' @export
ui_input.code_field <- function(x, id, name) {
  shinyAce::aceEditor(
    id,
    value(x),
    mode = "r",
    theme = "github",
    autoComplete = "live",
    fontSize = 14
  )
}

#' @export
ui_update.code_field <- function(x, session, id, name) {
  NULL
}

#' Code Block
#' @param data Dataset.
#' @param ... Ignored.
#' @name code_block
#' @export
new_code_transform_block <- function(...) {
  new_block(
    fields = list(
      code = new_code_field("data # from parent block")
    ),
    expr = quote({
      .(code) |>
        parse(text = _) |>
        eval()
    }),
    ...,
    class = c("code_transform_block", "transform_block", "submit_block")
  )
}

#' @rdname code_block
#' @export
new_code_plot_block <- function(...) {
  new_block(
    fields = list(
      code = new_code_field("plot(data) # from parent block")
    ),
    expr = quote({
      .(code) |>
        parse(text = _) |>
        eval()
    }),
    ...,
    class = c("code_plot_block", "plot_block", "submit_block")
  )
}

code_layout_fields <- function(x, fields, ...) {
  # we apply padding because there seems to be some
  # CSS that pushes the editor up and hides the buttons
  div(
    class = "mt-4",
    fields$code,
    fields$submit
  )
}

#' @export
layout.code_transform_block <- code_layout_fields

#' @export
layout.code_plot_block <- code_layout_fields
