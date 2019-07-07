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
                         digits = 2,
                         font.size = "10pt")
{
  
  # build summary
  summary_df <- data.frame(
    Variable    = names(df), 
    Description = sjlabelled::get_label(df), 
    psych::describe(df)
  ) %>% 
    dplyr::mutate(
      Missing = nrow(df) - n,
      Type    = map_chr(df, class)
    ) %>% 
    dplyr::select(
      Variable, 
      Description,
      Type,
      Obs. = n, 
      Missing,
      stats)
  
  # use numbers as rownames, corresponding to the column numbers
  rownames(summary_df) <- seq_along(summary_df$Variable)
  
  # if viewer = TRUE show as searchable datatable in the viewer pane
  if(viewer) {
    
    tempFileName <- tempfile("summary_df_", fileext = ".html")
    
    summary_df %>% 
      DT::datatable(
        extensions = 'Scroller',
        options = list(
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'font-size': '", font.size, "'});"),
            "}"),
          class = "compact",
          dom = 'fti',
          pageLength = nrow(summary_df),
          columnDefs = list(
            list(className = 'dt-left', targets = c(1,2))
            ),
          
          # for Scroller extension
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE
        ),
      ) %>% 
      DT::formatRound(columns = stats, digits = digits) %>%
      DT::formatStyle(columns = seq(0, length(summary_df)), fontSize = font.size) %>%
      DT::saveWidget(tempFileName)
    
    rstudioapi::viewer(tempFileName)
  }
  
  return(summary_df)
}
