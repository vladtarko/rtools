# rtools
Some useful functions in R

dataframe_summary <- **var.explorer**(dataframe)

> Creates a summary dataframe that can be used in RStudio similar to the variable explorer in Stata, but which also includes the summary statistics. This is useful particularly if you have a large dataset with a very large number of variables with hard to remember names. Can also be used to generate a table of summary statistics.

x_growth <- **growth.rate**(x, lag)

> Creates a growth rate variable. It doesn't require data to be time series.

**barplot_se**(m, se, group_labels, title) 

> Create a barplot with standard errors
> `m`: list of means, showed as higher or lower bar columns
> `se`: list of standard errors, showed as error bars attached to the columns
> `group_labels`: list of labels for each bar
> `title`: title of the barplot

legend <- **get.legend**(a.gplot)

> Gets the legend from a ggplot, which can then be added as a separate plot to a `grid.arrange`. This is useful if you have many plots with the same legend.
> Example:

```r
f1 <- ggplot() + 
        all the details of the first figure +
        theme(legend.position = "none")
f2 <- ggplot() + 
        all the details of the first figure
legend <- get.legend(f2)
f2 <- f2 +
        theme(legend.position = "none")
grid.arrange(f1, f2, legend)
```

dataframe <- **read_csv_with_labels**(fileName)

> Reads a csv file with variable names on the first row and the variable labels on the second row.

x_corr <- **cor.prob**(x)

> Creates a correlation matrix with p-values. Generates a table with all combinations of variables + a column with the correlation, and another column with the p-value. 
> _Original source:_ Stephen Turner, https://gist.github.com/stephenturner/3492773

dataframe_list <- **read_excel_allsheets**(filename, tibble = FALSE)

> Reads all sheets from Excel file into a list of dataframes. The names of each element in the list is the name of the Excel sheet. By default it creates list of dataframes, setting tibble to TRUE, creates list of tibbles.
> _Original source:_ https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames


**get_os**()

> Identifies your OS. Returns "win", "mac" or "unix".

**excel**(dataframe)

> Opens dataframe in Excel (for Windows) or LibreOffice (for Linux). I'm not sure how to add Mac support. 
> Solves the issue that large dataframes cannot be viewed in RStudio's with View().
> In Windows, temp .csv file saved in temporary folder (somewhere in "C:\Users\<YourName>\AppData\Local\Temp")
> In Linux, the temp file is saved in the home directory.
> _Original author:_ https://github.com/geneorama/geneorama/blob/master/R/wtf.R
> This function is changed from the original to use `fwrite` instead of `write.table`. It works MUCH faster this way. And it is changed to work for Linux as well.

