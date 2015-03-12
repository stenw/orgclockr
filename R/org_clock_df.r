org_clock_df <-
    function(x, units = "mins") {
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
                dplyr::summarise(TimeSpentPerDay  = sum(Time),
                                 AvgClockInterval = mean(Time) %>%
                                    round(2),
                                 NIntervals = n(),
                                 Period           = unique(Period)) %>%
                                     na.omit() %>%
                                     dplyr::ungroup()
    }
