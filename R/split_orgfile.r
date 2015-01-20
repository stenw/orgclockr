##' Split the org file by nodes.
##'
##' \code{split_orgfile} splits the org file by the index of the
##' headlines.
##' @param x org object as character vector.
##' @return the nodes of an org file as a character list.
##' @export split_orgfile
split_orgfile <-
    function(x) {
        headline_ids <-
            extract_headlines(orgfile) %>%
                complete.cases() %>%
                which()
        x %>%
            function(x) {
                unname(split(x, cumsum(seq_along(x) %in% headline_ids)))
            }
    }
