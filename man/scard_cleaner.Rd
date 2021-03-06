% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A_butler_ELEMENTS.R
\name{scard_cleaner}
\alias{scard_cleaner}
\title{Remove data (overwrite using an empty file several times) and THEN delete content of the copied SD cards.}
\usage{
scard_cleaner(x)
}
\arguments{
\item{x}{file_copy_status output}
}
\description{
Remove data (overwrite using an empty file several times) and THEN delete content of the copied SD cards.
}
\examples{
\dontrun{
x = file_copy_status(bid = boxid)
sdcard_uploader(x) # copy
scard_cleaner(x)   # empty files on cards when copied
}
}
\author{
MV
}
