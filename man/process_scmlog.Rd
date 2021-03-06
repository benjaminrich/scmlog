% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scmlog.R
\name{process_scmlog}
\alias{process_scmlog}
\title{Processes the scmlog.txt file and generates a Word document containing
tables for each forward inclusion and backwards elimination step.}
\usage{
process_scmlog(
  rundir = getwd(),
  input = file.path(rundir, "scmlog.txt"),
  output = file.path(rundir, "scmlog.docx"),
  template = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{rundir}{A directory containing the scmlog file (typically, the
directory that the scm command was run in).}

\item{input}{A file name for the input scm log file (typically, "scmlog.txt").}

\item{output}{A file name for the generated Word document.}

\item{template}{Optional. A Word document can be specified for use as a
template for the genrated output.}

\item{overwrite}{Should existing files be overwritten?}
}
\value{
Called for its side effects.
}
\description{
Processes the scmlog.txt file and generates a Word document containing
tables for each forward inclusion and backwards elimination step.
}
\examples{
\dontrun{
scmlog <- system.file("sample_scmlog.txt", package="scmlog")
process_scmlog(scmlog)  # Creates output in the current directory
}
}
