##' Extract the timestamps of an org file.
##'
##' This function is used to extract the timestamps matching the
##' regexp pattern.
##' @param orgfile org object as character vector.
##' @return the timestamps of an orgfile as a character matrix.
##' @export extract_tags
##' @seealso \code{extract_headlines}, \code{extract_tags}
##' and \code{extract_categories} for extractions of other org elements.
extract_timestamps <-
    function(orgfile) {
        orgfile %>%
            str_match(
                paste0(
                    "^.*CLOCK:\\ \\[[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]",
                    "{2}\\ [[:alpha:]]{2}\\ [[:digit:]]{2}:[[:digit:]]{2}\\]",
                    "--\\[[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",
                    "\\ [[:alpha:]]{2}\\ [[:digit:]]{2}:[[:digit:]]{2}\\]")) %>%
                        str_replace_all("(CLOCK:\\ \\[)|\\[|\\]", "") %>%
                        str_trim() %>%
                        str_split("--")
    }
