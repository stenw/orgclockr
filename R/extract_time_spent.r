##' Extract the time spent per headline in an org file.
##'
##' This function calculates the difference for each pair of clocking
##' timestamps and returns the sum of differences per headline. This
##' expects an even number of timestamps per headline, so in org files
##' with running clocks the lonely timestamp will be ignored.
##'
##' Org mode uses the standard ISO notation for dates and times as it
##' is defined in ISO 8601. The format is set in
##' \code{org-time-stamp-custom-formats}. If this variable is
##' modified, the extraction is likely to fail.
##' @param x org object as character vector.
##' @param units unit of time used.
##' @return a vector of doubles in the timeformat given by the
##' according parameter.
##' @export extract_time_spent
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_time_spent()
##' ##  [1]  NA  NA  21  NA  NA  57 232  NA 122 152   2 334
extract_time_spent <-
    function(x, units = "mins") {
        intervals <- extract_intervals(x, units = units)
        if (is.list(intervals)) {
            sapply(intervals, function(x) {
                sum(x) %>%
                    as.numeric() %>%
                    round(2)
            }) %>%
                unlist()
        } else {
            sum(intervals)
        }
    }
