% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table.R
\name{try_tex_compile}
\alias{try_tex_compile}
\title{MC table compilation}
\usage{
try_tex_compile(tex_f, pdf_file, chdir = TRUE, verbose = 1)
}
\arguments{
\item{tex_f}{The path (with .tex extension) to the input file.}

\item{pdf_file}{The path (with .pdf extension) to the output file.}

\item{chdir}{Change directory to where 'tex_f' is and perform compilation there on the base filename. Otherwise, if 'tex_f' has, e.g., a tilde in it, LaTeX compilation could fail.}

\item{verbose}{An integer (default of 1) to control the level feedback. Set to 0 to turn off messages that are not warnings or errors. At value 1, basic progress information is printed; at values 2 and higher, increasingly more messages are printed that are useful when debugging an unexpected result.}
}
\description{
MC table compilation
}
