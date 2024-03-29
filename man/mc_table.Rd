% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table.R
\name{mc_table}
\alias{mc_table}
\title{Output an MC to a .tex or .pdf table.}
\usage{
mc_table(
  diags_or_mc,
  output_file = NA,
  format = NA,
  engine = NA,
  aggregators,
  colname_poi,
  colname_stat,
  colname_diag,
  escape = FALSE,
  display_nsims = "row",
  rounder = gen_rounder_fixed(digits = 3),
  verbose = 1
)
}
\arguments{
\item{diags_or_mc}{The return from \code{\link{mc_diags}}. If instead you pass an MC object, the diagnostics will be run automatically if you specified the argument 'diagnostics' to mc_run().}

\item{output_file}{A vector of file names to output. Leave as NA (default) to print to the screen (useful when using knitr). The vector may have length greater than 1 if you want to output multiple formats. Currently, only ".tex" and ".pdf" are supported. If you specify a ".pdf" extension, you must have a TeX distribution installed (e.g., TeX Live or MiKTeX). If you don't have one installed, consider running tinytex::install_tinytex() in R, which installs a relatively lightweight (~250MB) TeX distribution.}

\item{format}{The format to output. One of "data.frame", "tex", or "pdf". The default is to guess the format from the extension of "output_file". If "output_file" is NA, then the default is "data.frame".}

\item{engine}{The engine used to generate the output. Currently supported engines are "montetools" and "gt". Engine "montetools" supports formats data.frame, tex, pdf. "gt" supports formats rtf, docx, tex, html, pdf, png.}

\item{aggregators}{What function should be used to summarize the diagnostics of each simulation (typically the default "mean").}

\item{colname_poi}{Set to NA to remove the column. By default, "param.", or if number of parameters is 1 it is NA.}

\item{colname_stat}{Set to NA to remove the column. By default, "stat.", or if number of statistics is 1 it is NA.}

\item{colname_diag}{Set to NA to remove the column. By default, "diag.", or if number of diagnostics is 1 it is NA.}

\item{escape}{If TRUE, escape special characters in LaTeX code. Also known as "sanitizing" the code.}

\item{display_nsims}{Can be "auto", "none", "row", or "multicol".}

\item{rounder}{The rounder to be used.}

\item{verbose}{An integer (default of 1) to control the level feedback. Set to 0 to turn off messages that are not warnings or errors. At value 1, basic progress information is printed; at values 2 and higher, increasingly more messages are printed that are useful when debugging an unexpected result.}
}
\description{
Output an MC to a .tex or .pdf table.
}
\details{
Return of "tex" format is a character vector. If you want to output directly to LaTeX, e.g., if calling mc_table() in knitr, wrap mc_table() in cat() and in the knitr chunk options specify "results = 'asis'". The point of returning a character vector is that you may choose to insert elements (which will be LaTeX lines after cat()) into the vector, such as "\\hline" to categorize certain rows.
mc_table supports output partial tables (e.g., if not all of the pn-chunks are available).
}
