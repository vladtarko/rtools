# creates a summary dataframe that can be used in RStudio --------------------- 
# similar to the variable explorer in Stata, 
# but which also includes the summary statistics.
#     * This is useful particularly if you have a large dataset 
#       with a very large number of variables with hard to remember names.
#     * Can also be used to generate a table of summary statistics.
#     * if `viewer` is TRUE (default) the result is shown in RStudio's Viewer pane
#       as a searchable datatable
vars_explore <- function(df, viewer = TRUE)
{
  
  library(dplyr)
  
  # build summary
  summary_df <- data.frame(
      Variable    = names(df), 
      Description = sjlabelled::get_label(df), 
      t(pastecs::stat.desc(df)) 
    ) %>% 
    dplyr::select(
      Variable, Description, 
      Obs. = nbr.val, Missing = nbr.na, 
      min, max, median, mean, std.dev)
  
  # use numbers as rownames, corresponding to the column numbers
  rownames(summary_df) <- seq_along(summary_df$Variable)
  
  # if viewer = TRUE show as searchable datatable in the viewer pane
  if(viewer) {
    
    tempFileName <- tempfile("summary_df_", fileext = ".html")
    
    summary_df %>% 
      DT::datatable() %>% 
      DT::formatRound(columns = 5:9, digits = 2) %>% 
      DT::saveWidget(tempFileName)
    
    rstudioapi::viewer(tempFileName)
  }
  
  return(summary_df)
}
