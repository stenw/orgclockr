##' Aggregate the clocking information of an org file.
##'
##' This function creates a \code{dplyr::data_frame} object that
##' aggregates the clocking information in each heading. In addition,
##' the period between the first and last timestamp stored per task is
##' returned in the 'Period' column. The values in 'Period' disregard
##' the 'Date' and should be seen more generally as a measure of how
##' much time this task took to complete. It's the time spanning from
##' the first time ever clocked in and the last time ever clocked out.
##' The values in 'TimeSpent' tell how much time in total a task took
##' on a given day. For simplicity reasons, intervals are not split at
##' midnight. Keep this in mind when clocking for long periods of time
##' spanning from one day to the next. The 'AvgClockInterval' is a
##' measure of how long on average a task has been clocked on the day
##' given in the 'Date' column. The 'NIntervals' column returns the
##' number of clock intervals for the day on a task. The unit of time
##' in the columns 'TimeSpent', 'AvgClockInterval' and 'Period' is
##' controlled by the \code{units} parameter.
##'
##' Org mode uses the standard ISO notation for dates and times as it
##' is defined in ISO 8601. The format is set in
##' \code{org-time-stamp-custom-formats}. If this variable is
##' modified, the extraction of 'Dates' is likely to fail.
##' @param x org object as character vector.
##' @param units unit of time used in the columns 'TimeSpent',
##' 'AvgClockInterval' and 'Period'.
##' @param avg_fun the function to calculate the values in
##' 'AvgClockInterval'. Should be either \code{mean} or \code{median}.
##' @return a \code{dplyr::data_frame} object.
##' @export org_clock_df
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' org_clock_df() %>%
##' head(4)
##' ## Source: local data frame [4 x 6]
##' ##
##' ##    Headline       Date TimeSpent AvgClockInterval NIntervals Period
##' ## 1 TaskEight 2015-01-20       129           129.00          1  23085
##' ## 2 TaskEight 2015-02-05        23            11.50          2  23085
##' ## 3  TaskFive 2015-02-28        51            12.75          4   1291
##' ## 4  TaskFive 2015-03-01         6             3.00          2   1291
##' @seealso \code{extract_intervals}, \code{extract_deadlines},
##' \code{extract_time_spent}, \code{extract_categories},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags},
##' \code{extract_efforts} and \code{extract_levels} to extract
##' particular org elements. Use \code{org_elements_df} to extract
##' various org elements.
org_clock_df <-
    function(x, units = "mins", avg_fun = "mean") {
        intervals       <- extract_intervals(x, units = units)
        timestamps      <- extract_timestamps(x)
        timestamps_date <- timestamps %>%
            lapply(function(x) strptime(x, "%Y-%m-%d %A %H:%M"))
        headlines       <- extract_headlines(x)
        repeated        <- sapply(intervals, length)
        dplyr::data_frame(
            Headline =
                rep(headlines, times = repeated),
            Date =
                sapply(timestamps_date, function(x) {
                    if (length(x) > 1) {
                        odd <- seq(1, length(x), by = 2)
                        x[odd] %>%
                            trunc("days")
                    } else {
                        NA
                    }
                }) %>%
                    ## unlisting won't work in date format
                    sapply(as.character) %>%
                    unlist() %>%
                    as.Date(Date, format = "%Y-%m-%d"),
            Time =
                intervals %>%
                unlist(),
            First =
                lapply(timestamps, last) %>% unlist() %>%
                rep(times = repeated),
            Last =
                lapply(timestamps, first) %>% unlist() %>%
                rep(times = repeated),
            Period =
                difftime(strptime(Last, format = "%Y-%m-%d %A %H:%M"),
                         strptime(First, format = "%Y-%m-%d %A %H:%M"),
                         units = units) %>%
                             as.numeric() %>%
                             ## If there is only one pair of
                             ## timestamps, the last timestamp
                             ## is actually in the First
                             ## variable. Circumventing this by
                             ## returning the absolute value
                             abs() %>%
                             round(2)
            ) %>%
                dplyr::group_by(Headline, Date) %>%
                dplyr::summarise(TimeSpent        = sum(Time),
                                 AvgClockInterval = do.call(avg_fun,
                                     list(Time)) %>%
                                         round(2),
                                 NIntervals = n(),
                                 Period           = unique(Period)) %>%
                                     na.omit() %>%
                                     dplyr::ungroup()
    }
