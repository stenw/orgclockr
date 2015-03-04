##' Split the org file by nodes.
##'
##' \code{split_orgfile} splits the org file by the index of the
##' headlines.
##' @param x org object as character vector.
##' @return the nodes of an org file as a character list.
split_orgfile <-
    function(x) {
        headline_ids <-
            x %>%
                extract_raw_headlines() %>%
                complete.cases() %>%
                which()
        split_file <-
            x %>%
                function(x) {
                    unname(split(x, cumsum(seq_along(x) %in% headline_ids)))
                }
        if (length(headline_ids) > 0 && headline_ids[1] != 1) {
            return(split_file[2:length(split_file)])
        } else {
            return(split_file)
        }
    }
