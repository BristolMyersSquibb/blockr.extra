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

register_blockr_extra_blocks <- function(pkg) {

  if (missing(pkg)) {
    pkg <- pkg_name()
  }

  register_blocks(
    constructor = c(
      plot_block,
      ggiraph_block,
      asfactor_block,
      lab_data_block,
      demo_data_block,
      demo_join_block,
      demo_arrange_block,
      demo_group_by_block,
      demo_filter_block_1,
      demo_filter_block_2,
      demo_summarize_block,
      admiral_dpc_block,
      filter_expr_block,
      summarize_expr_block
    ),
    name = c(
      "plot block",
      "ggiraph block",
      "asfactor block",
      "lab data_block",
      "demo data block",
      "demo join block",
      "demo arrange block",
      "demo group by block",
      "demo filter block 1",
      "demo filter block 2",
      "demo summarize block",
      "admiral dpc block",
      "filter expr block",
      "summarize expr block"
    ),
    description = c(
      "Monolithic ggplot block",
      "Monolithic ggiraph block",
      "As factor mutate block",
      "blockr.data preselected to lab data",
      "blockr.data preselected to demo data",
      "Predefined join block for BMS demo",
      "Predefined arrange block for BMS demo",
      "Predefined group by block for BMS demo",
      "Predefined filter block for BMS demo 1",
      "Predefined filter block for BMS demo 2",
      "Predefined summarize block for BMS demo",
      "Admiral block",
      "Filter expr block",
      "Summarize expr block"
    ),
    classes = list(
      c("plot_block", "submit_block"),
      c("ggiraph_block", "plot_block", "submit_block"),
      c("asfactor_block", "transform_block"),
      c("lab_dataset_block", "data_block"),
      c("demo_dataset_block", "data_block"),
      c("demo_join_block", "transform_block", "submit_block"),
      c("demo_arrange_block", "transform_block"),
      c("demo_group_by_block", "transform_block"),
      c("demo_filter_block_1", "transform_block", "submit_block"),
      c("demo_filter_block_2", "transform_block", "submit_block"),
      c("demo_summarize_block", "transform_block", "submit_block"),
      c("admiral_dpc_block", "transform_block"),
      c("filter_expr_block", "transform_block"),
      c("summarize_expr_block", "transform_block")
    ),
    input = c(
      "data.frame",
      "data.frame",
      "data.frame",
      NA_character_,
      NA_character_,
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame"
    ),
    output = c(
      "list",
      "list",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame",
      "data.frame"
    ),
    package = pkg
  )
}
