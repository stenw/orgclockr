extract_timestamps <-
    function(x) {
        split_file <- split_orgfile(x)
        timestamps <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    extract_timestamps_total() %>%
                    unlist() %>%
                    na.omit() %>%
                    stringr::str_c()
            })
        timestamps[which(timestamps == "")] <- NA
        ## unlist(timestamps) %>%
        return(timestamps)
    }
