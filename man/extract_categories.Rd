% Generated by roxygen2 (4.0.1): do not edit by hand
\name{extract_categories}
\alias{extract_categories}
\title{Extract the categories of an org file.}
\usage{
extract_categories(x)
}
\arguments{
\item{x}{org object as character vector.}
}
\value{
the categories of an orgfile as a character vector.
}
\description{
This function is used to extract the categories matching the
regexp pattern.
}
\examples{
system.file("extdata", "sample.org", package = "orgclockr") \%>\%
readLines() \%>\%
extract_categories() \%>\%
first()
## [1] "CategoryOne"
}

