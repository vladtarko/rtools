# Reads all sheets from an Excel file into a list of tibbles/dataframes
# The names of each element in the list is the name of the excel sheet.
# Source: https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
# Parameter `tibble` determines whether the return is a tibble or just a regular dataframe.
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) { readxl::read_excel(filename, sheet = X) })
  if(!tibble) { x <- lapply(x, as.data.frame) }
  names(x) <- sheets
  return(x)
}
