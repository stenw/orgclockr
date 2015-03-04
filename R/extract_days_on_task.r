##' Extract the number of days a task has been worked on.
##'
##' Do not confuse the number of days on a task with the total time
##' spent on a task in days. The latter can be calculated from
##' \\code{extract_time_spent}.
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
