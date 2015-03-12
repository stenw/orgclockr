##' Extract the number of days a task has been worked on.
##'
##' Do not confuse the number of days on a task with the total time
##' spent on a task in days. The latter can be calculated from
##' \\code{extract_time_spent}.
##'
##' Org mode uses the standard ISO notation for dates and times as it
##' is defined in ISO 8601. The format is set in
##' \code{org-time-stamp-custom-formats}. If this variable is
##' modified, the extraction is likely to fail.
##' @param x
##' @return the days on a task in an org file as a numeric vector.
##' @export extract_days_on_task
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_days_on_task()
##' ##  [1] 0 0 2 0 0 1 5 0 1 2 1 2
##' @seealso \code{extract_days_on_task}, \code{extract_time_spent},
##' \code{extract_categories}, \code{extract_todostates},
##' \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_tags}, \code{extract_efforts} and
##' \code{extract_levels} to extract other org elements.
extract_days_on_task <-
    function(x) {
        x %>%
            extract_timestamps() %>%
            sapply(function(x) {
                as.Date(x) %>%
                    unique() %>%
                    length()
            })
    }
