register_extra_blocks <- function(pkgname) {
  blockr::register_block(
    constructor = code_transform_block,
    name = "Code transform block",
    description = "Use arbitrary R code to transform data",
    classes = c("code_transform_block", "transform_block"),
    input = "data.frame",
    output = "data.frame"
  )

  blockr::register_block(
    constructor = code_plot_block,
    name = "Code plot block",
    description = "Use arbitrary R code to plot data",
    classes = c("code_plot_block", "transform_block"),
    input = "data.frame",
    output = "data.frame"
  )

  blockr::register_block(
    constructor = summarize_expr_block,
    name = "Summarize expression block",
    description = "Use arbitrary R code to summarize data",
    classes = c("summarize_expr_block", "transform_block"),
    input = "data.frame",
    output = "data.frame"
  )

  blockr::register_block(
    constructor = filter_expr_block,
    name = "Filter expression block",
    description = "Use arbitrary R code to filter data",
    classes = c("filter_expr_block", "transform_block"),
    input = "data.frame",
    output = "data.frame"
  )

  blockr::register_block(
    constructor = admiral_dpc_block,
    name = "Admiral DPC block",
    description = "Admiral derive param computed block",
    classes = c("admiral_dpc_block", "transform_block"),
    input = "data.frame",
    output = "data.frame"
  )
}
