extract_categories <-
    function(x) {
        split_file <- split_orgfile(x)
        categories <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    extract_categories_total() %>%
                    ## should concatenate the categories per node. not
                    ## sure if more categories per node are reasonable
                    ## in orgmode though.
                    paste0(collapse = " ")
            })
        categories[which(categories == "")] <- NA
        unlist(categories) %>%
        return()
    }
