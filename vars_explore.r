#' Searchable variable explorer with labeled variables in RStudio's Viewer Pane.
#' 
#' Creates a summary dataframe that can be used in RStudio similar to the variable 
#' explorer in Stata, but which also includes the summary statistics. If `viewer` 
#' is TRUE (default) the result is shown in RStudio's Viewer pane as a searchable 
#' datatable. 
#' 
#' This is useful particularly if you have a large dataset with a very large number
#' of labelled variables with hard to remember names. Can also be used to generate 
#' a table of summary statistics. 
#'
#' @param df A data frame.
#' @param stats A string vector. What statistics to show. Default is
#'        c("mean", "median", "sd", "min", "max"). Available stats: 
#'        "mean", "sd" = standard deviation, "trim" = trimmed mean, 
#'        "median", "mad" = median absolute deviation (from the median), 
#'        "min" = minimum, "max" = maximum, "skew", "kurtosis", 
#'        "se" = standard error.
#' @param viewer Logical. Whether to show results as a searchable datatable
#'        in RStudio's Viewer pane. Default is TRUE.
#' @param digits Numeric. How many digits to show for the statistics. 
#'        Default is 2.
#'
#' @return A data frame with the summary statistics, each variable on a row, 
#'        each statistic as a column.
#'
#' @examples
#' 
#' qog <- rio::import("http://www.qogdata.pol.gu.se/dataarchive/qog_bas_cs_jan18.dta")
#' qog_summary <- vars_explore(qog)
#' 
vars_explore <- function(df, 
                         stats = c("mean", "median", "sd", "min", "max"), 
                         viewer = TRUE,
                         digits = 2)
{
  
  # build summary
  summary_df <- data.frame(
      Variable    = names(df), 
      Description = sjlabelled::get_label(df), 
      psych::describe(df)
    ) %>% 
    dplyr::mutate(
      Missing = max(n) - n) %>% 
    dplyr::select(
      Variable, 
      Description, 
      Obs. = n, 
      Missing,
      stats)
    
  # use numbers as rownames, corresponding to the column numbers
  rownames(summary_df) <- seq_along(summary_df$Variable)
  
  # if viewer = TRUE show as searchable datatable in the viewer pane
  if(viewer) {
    
    tempFileName <- tempfile("summary_df_", fileext = ".html")
    
    summary_df %>% 
      DT::datatable() %>% 
      DT::formatRound(columns = seq(5, length(stats) + 4), 
                      digits = digits) %>% 
      DT::saveWidget(tempFileName)
    
    rstudioapi::viewer(tempFileName)
  }
  
  return(summary_df)
}
