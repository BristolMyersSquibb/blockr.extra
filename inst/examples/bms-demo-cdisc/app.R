# Basic example
library(shiny)
library(blockr)
library(blockr.data)
library(ggiraph)

# Note: sometimes memories issue due to the join block
# can be solved by restarting R between each run.
serve_workspace(
  stack1 = new_stack(lab_data_block, head_block),
  stack2 = new_stack(demo_data_block, demo_join_block),
  stack3 = new_stack(
    result_block,
    demo_filter_block_1,
    demo_filter_block_2#,
    #demo_arrange_block,
    #asfactor_block#,
    #demo_group_by_block,
    #demo_summarize_block,
    #ggiraph_block
  ),
  title = "My workspace"
)
