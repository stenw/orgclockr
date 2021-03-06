% Generated by roxygen2 (4.0.1): do not edit by hand
\name{extract_headlines}
\alias{extract_headlines}
\title{Extract the headlines of an org file.}
\usage{
extract_headlines(x)
}
\arguments{
\item{x}{org object as character vector.}
}
\value{
the headlines of an orgfile as a character matrix.
}
\description{
This function is used to extract the headlines matching the regexp
pattern.
}
\examples{
system.file("extdata", "sample.org", package = "orgclockr") \%>\%
readLines() \%>\%
extract_headlines() \%>\%
last()
## [1] "TaskTen"
}

