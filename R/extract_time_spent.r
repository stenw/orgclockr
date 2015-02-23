extract_time_spent <-
    function(x, units = "mins") {
        extract_timestamps(x) %>%
            sapply(function(x) strptime(x, "%Y-%m-%d %A %H:%M")) %>%
            sapply(function(x) {
                if (length(x) > 1) {
                    odd <- seq(1, length(x), by = 2)
                    diff(x)[odd] %>%
                        sum() %>%
                        as.numeric(units = units)
                } else {
                    0
                }
            }) %>%
                unlist()
    }
