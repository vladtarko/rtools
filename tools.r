# Some useful functions

## libraries used by some of the functions (some require install) -------------------
library(pastecs)
library(Hmisc)
library(sjlabelled)
library(data.table)
library(readxl)
library(dplyr)

# growth rate -----------------------------------------------------------------------

growth.rate <- function(x, lag = 1) {
  
  return((x - dplyr::lag(x, lag)) / dplyr::lag(x, lag) * 100)
  
}


# calculate the standard error ------------------------------------------------------
std.err <- function(x) 
{
  sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
}

# create a base r barplot with standard errors ---------------------------------------------
# m: list of means, showed as higher or lower bar columns
# se: list of standard errors, showed as error bars attached to the columns
# group_labels: list of labels for each bar
# title: title of the barplot
barplot_se <- function (m, se, group_labels, title) 
{
  # calculate how much to extend the plot upwards to show the error bars properly
  plotTop <- max(m) + 2 * max(se)
  
  # create the barplot
  bplot <- barplot(m, names.arg = group_labels, main=title, ylim = c(0, plotTop))
  
  # create the error bars
  segments(bplot, m + 2 * se,
           bplot, m - 2 * se)
  
  arrows(bplot, m + 2 * se,
         bplot, m - 2 * se, 
         lwd = 1.5, angle = 90, code = 3, length = 0.05)
}

# get the legend from a ggplot
get.legend <- function(a.gplot) 
{
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}




# Correlation matrix with p-values. -----------------------------------------------
# See http://goo.gl/nahmV for documentation of this function
# Source: Stephen Turner, https://gist.github.com/stephenturner/3492773
cor.prob <- function (X, dfr = nrow(X) - 2) 
{
  R        <- cor(X, use="pairwise.complete.obs")
  above    <- row(R) < col(R)
  r2       <- R[above]^2
  Fstat    <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  return(flattenSquareMatrix(R))
}

# Use this to dump the cor.prob output to a 4 column matrix
# with row/column indices, correlation, and p-value.
# See StackOverflow question: http://goo.gl/fCUcQ
# Source: Stephen Turner, https://gist.github.com/stephenturner/3492773
flattenSquareMatrix <- function(m) 
{
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i   = rownames(m)[row(m)[ut]],
             j   = rownames(m)[col(m)[ut]],
             cor = t(m)[ut],
             p   = m[ut])
}


# read all sheets from excel file into a list of dataframes ------------------------------------
# the names of each element in the list is the name of the excel sheet
# source: https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) { readxl::read_excel(filename, sheet = X) })
  if(!tibble) { x <- lapply(x, as.data.frame) }
  names(x) <- sheets
  return(x)
}


# identify your OS -----------------------------------------
get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}



# Opens dataframe in Excel (for Windows) or LibreOffice (for Linux) -------------------------
# Solves the issue that large dataframes cannot be viewed in RStudio's with View().
# In Windows, temp .csv file saved in temporary folder 
# (somewhere in "C:\Users\<YourName>\AppData\Local\Temp")
# In Linux, the temp file is saved in the home directory.
# author: https://github.com/geneorama/geneorama/blob/master/R/wtf.R
# This function is changed from the original to use `fwrite` instead of `write.table`
# it works MUCH faster this way.
excel <- function (x) {
  if (get_os() == "win") {
    tempFilePath = paste(tempfile(), ".csv")
    tempPath = dirname(tempFilePath)
  } else {
    tempPath = "~"
  }
  
  preferredFile = paste(deparse(substitute(x)), ".csv", sep = "")
  preferredFilePath = file.path(tempPath, preferredFile)
  
  if(length(dim(x))>2){
    stop('Too many dimensions')
  }
  if(is.null(dim(x))){
    x = as.data.frame(x)
  }
  if (is.null(rownames(x))) {
    tmp = 1:nrow(x)
  } else {
    tmp = rownames(x)
  }
  rownames(x) = NULL
  x = data.frame(RowLabels = tmp, x)
  WriteAttempt = try(
    data.table::fwrite(x, file=preferredFilePath, quote=TRUE),
    silent = TRUE)
  if (get_os() == "win") {
    if ("try-error" %in% class(WriteAttempt)) {
      data.table::fwrite(x, file=tempFilePath, quote=TRUE)
      shell.exec(tempFilePath)
    } else {
      shell.exec(preferredFilePath)
    }
  } else {
    if ("try-error" %in% class(WriteAttempt)) {
      data.table::fwrite(x, file=tempFilePath, quote=TRUE)
      system(paste("libreoffice", tempFilePath))
    } else {
      system(paste("libreoffice", preferredFilePath))
    }
  }
}
