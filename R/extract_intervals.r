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
