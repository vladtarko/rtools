# reads a csv file with variable names on the first row -------------------------------
# and the variable labels on the second row
read_csv_with_labels <- function(fileName)
{
  # read the first two lines
  varNames  <- read.csv(fileName, nrows = 1, stringsAsFactors = FALSE, header = FALSE)
  varLabels <- read.csv(fileName, nrows = 1, stringsAsFactors = FALSE, header = TRUE)
  
  # read the data
  df <- read.csv(fileName, skip = 2)
  
  # assign variable names and labels
  names(df) <- varNames
  Hmisc::label(df) <- varLabels 
  
  return(df)
}
