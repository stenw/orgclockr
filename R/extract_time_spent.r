##' Extract the time spent per headline in an org file.
##'
##' This function calculates the difference for each pair of clocking
##' timestamps and returns the sum of differences per headline. This
##' expects an even number of timestamps per headline, so in org files
##' with running clocks the missing timestamps will be ignored.
##' @param x org object as character vector.
##' @param units unit of time used.
##' @return a vector of doubles in the timeformat given by the
##' according parameter.
##' @export extract_time_spent
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_time_spent()
##' ##  [1]   0   0  21   0   0   2 232   0 122 152   2 334
##' @seealso \code{extract_levels}, \code{extract_efforts},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags} and
##' \code{extract_categories} to extract other org elements.
extract_time_spent <-
    function(x, units = "mins") {
        extract_timestamps(x) %>%
            sapply(function(x) strptime(x, "%Y-%m-%d %A %H:%M")) %>%
            sapply(function(x) {
                if (length(x) > 1) {
                    odd <- seq(1, length(x), by = 2)
                    diff(x)[odd] %>%
                        sum() %>%
                        as.numeric(units = units) %>%
                        round(2)
                } else {
                    0
                }
            }) %>%
                unlist()
    }
