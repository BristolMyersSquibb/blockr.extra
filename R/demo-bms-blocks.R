#' Monolithic ggplot block
#'
#' Used with cdisc data. See \link{demo_data_block}.
#'
#' @inheritParams blockr::server_output
#' @inheritParams blockr::uiOutputBlock
#' @param data Tabular data in which to select some columns.
#' @param plot_opts List containing options for ggplot (color, ...).
#' @param ... Any other params. TO DO
#' @rdname plot_block
#' @import ggplot2
#' @import blockr
#' @export
new_plot_block <- function(
    data,
    plot_opts = list(
      colors = c("blue", "red"), # when outside aes ...
      point_size = 3,
      title = "Plot title",
      theme = c(
        "theme_minimal",
        "theme_gray",
        "theme_linedraw",
        "theme_dark",
        "theme_light",
        "theme_classic",
        "theme_void",
        "theme_bw"
      ),
      x_lab = "X axis label",
      y_lab = "Y axis label",
      errors = list(
        show = FALSE,
        ymin = character(),
        ymax = character()
      ),
      lines = list(
        show = FALSE,
        group = character(),
        color = character()
      )
    ),
    ...) {
  # For plot blocks, fields will create input to style the plot ...
  all_cols <- function(data) colnames(data)
  fields <- list(
    x_var = new_select_field("VISIT", all_cols),
    y_var = new_select_field("MEAN", all_cols),
    color = new_select_field("ACTARM", all_cols),
    shape = new_select_field("ACTARM", all_cols),
    point_size = new_range_field(plot_opts$point_size, min = 1, max = 10),
    title = new_string_field(plot_opts$title),
    x_lab = new_string_field(plot_opts$x_lab),
    y_lab = new_string_field(plot_opts$y_lab),
    theme = new_select_field(plot_opts$theme[[1]], plot_opts$theme),
    errors_toggle = new_switch_field(plot_opts$errors$show),
    lines_toggle = new_switch_field(plot_opts$lines$show)
  )

  new_block(
    fields = fields,
    expr = quote({
      x_var <- .(x_var)
      y_var <- .(y_var)
      color <- .(color)
      shape <- .(shape)
      ymin <- "ymin"
      ymax <- "ymax"

      p <- ggplot(data) +
        geom_point(
          # We have to use aes_string over aes
          mapping = aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            color = .data[[color]],
            shape = .data[[shape]]
          ),
          size = 3 # .(point_size) TO DO: allow slide to have 1 value
        )

      # Adding errors
      if (.(errors_toggle)) {
        p <- p + geom_errorbar(
          aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            ymin = MEAN - SE,
            ymax = MEAN + SE,
            color = ACTARM
          ),
          width = 0.2
        )
      }

      if (.(lines_toggle)) {
        p <- p + geom_line(
          aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            group = .data[[color]],
            color = .data[[color]]
          )
        )
      }

      p +
        labs(
          title = .(title),
          x = .(x_lab),
          y = .(y_lab)
        ) +
        # theme_update(.(theme)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom"
        ) +
        scale_color_brewer(name = "Treatment Group", palette = "Set1") +
        scale_shape_manual(
          name = "Treatment Group",
          values = c(16, 17, 18, 19, 20)
        )
    }),
    ...,
    class = c("plot_block")
  )
}

#' @rdname plot_block
#' @export
plot_block <- function(data, ...) {
  initialize_block(new_plot_block(data, ...), data)
}

#' Monolithic ggiraph plot block
#'
#' To use with \link{demo_data_block} as data input.
#'
#' @param data Tabular data in which to select some columns.
#' @param plot_opts List containing options for ggplot (color, ...).
#' @param ... Any other params. TO DO
#' @rdname plot_block
#' @import ggiraph
#' @export
new_ggiraph_block <- function(
    data,
    plot_opts = list(
      colors = c("blue", "red"), # when outside aes ...
      point_size = 3,
      title = "Plot title",
      theme = c(
        "theme_minimal",
        "theme_gray",
        "theme_linedraw",
        "theme_dark",
        "theme_light",
        "theme_classic",
        "theme_void",
        "theme_bw"
      ),
      x_lab = "X axis label",
      y_lab = "Y axis label",
      errors = list(
        show = TRUE,
        ymin = character(),
        ymax = character()
      ),
      lines = list(
        show = TRUE,
        group = character(),
        color = character()
      )
    ),
    ...) {
  # For plot blocks, fields will create input to style the plot ...
  all_cols <- function(data) colnames(data)
  fields <- list(
    x_var = new_select_field("VISIT", all_cols),
    y_var = new_select_field("MEAN", all_cols),
    color = new_select_field("ACTARM", all_cols),
    shape = new_select_field("ACTARM", all_cols),
    point_size = new_range_field(plot_opts$point_size, min = 1, max = 10),
    title = new_string_field(plot_opts$title),
    x_lab = new_string_field(plot_opts$x_lab),
    y_lab = new_string_field(plot_opts$y_lab),
    errors_toggle = new_switch_field(plot_opts$errors$show),
    lines_toggle = new_switch_field(plot_opts$lines$show)
  )

  new_block(
    fields = fields,
    expr = quote({
      x_var <- .(x_var)
      y_var <- .(y_var)
      color <- .(color)
      shape <- .(shape)

      data <- data |>
        mutate(
          ymin = MEAN - SE,
          ymax = MEAN + SE,
          TOOLTIP = sprintf("x: %s\ny: %s", .data[[x_var]], .data[[y_var]]),
          TOOLTIP_SE = sprintf(
            "x: %s\ny: %s\nmin: %s\nmax: %s",
            .data[[x_var]], .data[[y_var]],
            ymin, ymax
          )
        )

      p <- ggplot(data) +
        ggiraph::geom_point_interactive(
          # We have to use aes_string over aes
          mapping = aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            color = .data[[color]],
            shape = .data[[shape]],
            tooltip = TOOLTIP
          ),
          size = 3 # .(point_size) TO DO: allow slide to have 1 value
        )

      # Adding errors
      if (.(errors_toggle)) {
        p <- p + ggiraph::geom_errorbar_interactive(
          aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            ymin = MEAN - SE,
            ymax = MEAN + SE,
            color = ACTARM,
            tooltip = TOOLTIP_SE
          ),
          width = 0.2
        )
      }

      if (.(lines_toggle)) {
        p <- p + ggiraph::geom_line_interactive(
          aes(
            x = .data[[x_var]],
            y = .data[[y_var]],
            group = .data[[color]],
            color = .data[[color]]
          )
        )
      }

      p <- p +
        labs(
          title = .(title),
          x = .(x_lab),
          y = .(y_lab)
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom"
        ) +
        ggiraph::scale_color_brewer_interactive(
          name = "Treatment Group",
          palette = "Set1"
        ) +
        ggiraph::scale_shape_manual_interactive(
          name = "Treatment Group",
          values = c(16, 17, 18, 19, 20)
        )

      p <- ggiraph::girafe(ggobj = p)
      p <- ggiraph::girafe_options(
        p,
        ggiraph::opts_tooltip(
          opacity = .7,
          offx = 20,
          offy = -10,
          use_fill = TRUE,
          use_stroke = TRUE,
          delay_mouseout = 1000
        )
      )
    }),
    ...,
    class = c("ggiraph_block", "plot_block")
  )
}

#' @rdname plot_block
#' @export
ggiraph_block <- function(data, ...) {
  initialize_block(new_ggiraph_block(data, ...), data)
}

#' @rdname plot_block
#' @export
server_output.ggiraph_block <- function(x, result, output) {
  renderGirafe(result())
}

#' @rdname plot_block
#' @export
uiOutputBlock.ggiraph_block <- function(x, ns) {
  girafeOutput(ns("plot"))
}

#' As factor block
#'
#' Useful for the BMS demo app
#'
#' @inheritParams new_plot_block
#' @param column Column to apply the operation on.
#' @rdname demo_block
#' @export
#' @import blockr
new_asfactor_block <- function(data, column = "VISIT", ...) {
  all_cols <- function(data) colnames(data)

  mutate_expr <- function(data, column) {
    if (is.null(column)) {
      return(NULL)
    }
    if (!(column %in% colnames(data))) {
      return(NULL)
    }

    bquote(
      dplyr::mutate(
        VISIT = factor(
          .(column),
          levels = unique(.(column)),
          ordered = TRUE
        )
      ),
      list(column = as.name(column))
    )
  }

  fields <- list(
    column = new_select_field(column, column),
    expression = new_hidden_field(mutate_expr)
  )

  new_block(
    fields = fields,
    expr = quote(.(expression)),
    ...,
    class = c("asfactor_block", "transform_block")
  )
}

#' @rdname demo_block
#' @export
asfactor_block <- function(data, ...) {
  initialize_block(new_asfactor_block(data, ...), data)
}

#' @rdname demo_block
#' @export
demo_data_block <- function(...) {
  initialize_block(
    new_data_block(
      ...,
      dat = as.environment("package:blockr.data"),
      selected = "lab"
    )
  )
}

#' @rdname demo_block
#' @export
demo_join_block <- function(data, ...) {
  initialize_block(
    new_join_block(
      data,
      y = "demo",
      type = "inner",
      by_col = c("STUDYID", "USUBJID"),
      ...
    ),
    data
  )
}

#' @rdname demo_block
#' @export
demo_arrange_block <- function(data, ...) {
  arrange_block(
    data,
    columns = "VISITNUM",
    ...
  )
}

#' @rdname demo_block
#' @export
demo_group_by_block <- function(data, ...) {
  group_by_block(
    data,
    columns = c("VISIT", "ACTARM"),
    ...
  )
}

#' @rdname demo_block
#' @export
demo_filter_block_1 <- function(data, ...) {
  initialize_block(
    new_filter_block(
      data,
      columns = "LBTEST",
      values = "Hemoglobin",
      ...
    ),
    data
  )
}

#' @rdname demo_block
#' @export
demo_filter_block_2 <- function(data, ...) {
  initialize_block(
    new_filter_block(
      data,
      columns = "VISIT",
      values = "UNSCHEDULED",
      filter_fun = "!startsWith",
      ...
    ),
    data
  )
}

#' @rdname demo_block
#' @export
demo_summarize_block <- function(data, ...) {
  initialize_block(
    new_summarize_block(
      data,
      default_columns = c("LBSTRESN", "LBSTRESN"),
      ...
    ),
    data
  )
}