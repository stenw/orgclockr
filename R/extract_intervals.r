##' Extract clock intervals of headings in an org file.
##'
##' This expects an even number of timestamps per headline, so in org
##' files with running clocks the lonely timestamp will be ignored.
##'
##' Org mode uses the standard ISO notation for dates and times as it
##' is defined in ISO 8601. The format is set in
##' \code{org-time-stamp-custom-formats}. If this variable is
##' modified, the extraction is likely to fail.
##' @param x org object as character vector.
##' @param units unit of time used.
##' @return a list of double values in the unit of time given.
##' @export extract_intervals
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_intervals() %>%
##' last()
##' ## [1]  68  17   1  62  16 170
extract_intervals <-
    function(x, units = "mins") {
        x %>%
            extract_timestamps() %>%
            sapply(function(x) strptime(x, "%Y-%m-%d %A %H:%M")) %>%
            sapply(function(x) {
                if (length(x) > 1) {
                    odd <- seq(1, length(x), by = 2)
                    diff(x)[odd] %>%
                        as.numeric(units = units) %>%
                        round(2)
                } else {
                    NA
                }
            })
    }
