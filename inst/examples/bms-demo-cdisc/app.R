# Basic example
library(shiny)
library(blockr)
library(blockr.data)
library(ggiraph)

# Note: sometimes memories issue due to the join block
# can be solved by restarting R between each run.
serve_workspace(
  stack1 = new_stack(new_dataset_block("lab", package = "blockr.data"), new_head_block),
  stack2 = new_stack(new_dataset_block("demo", package = "blockr.data"), new_join_block(
    y = "demo",
    type = "inner",
    by_col = c("STUDYID", "USUBJID")
  )),
  stack3 = new_stack(
    new_result_block,
    new_filter_block(
      columns = "LBTEST",
      values = "Hemoglobin"
    ),
    new_filter_block(
      columns = "VISIT",
      values = "UNSCHEDULED",
      filter_fun = "!startsWith"
    ),
    new_arrange_block(columns = "VISITNUM"),
    #new_asfactor_block,
    new_group_by_block(columns = c("VISIT", "ACTARM"))#,
    #new_summarize_block(func = c(), default_columns = c("LBSTRESN", "LBSTRESN"))
  ),
  title = "My workspace"
)
