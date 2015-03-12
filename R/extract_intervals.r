##' Extract clock intervals of headings in an org file.
##'
##' This expects an even number of timestamps per headline, so in org
##' files with running clocks the lonely timestamp will be ignored.
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
##' @seealso \code{extract_time_spent}, \code{extract_days_on_task},
##' \code{extract_levels}, \code{extract_efforts},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags} and
##' \code{extract_categories} to extract other org elements.
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
