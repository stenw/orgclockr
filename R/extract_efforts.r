extract_efforts <-
    function(x) {
        split_file <- split_orgfile(x)
        efforts <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    extract_efforts_total() %>%
                    paste0(collapse = " ")
            })
        efforts[which(length(efforts) < 1)] <- NA
        unlist(efforts) %>%
            return()
    }
