##' Extract the timestamps of an org file.
##'
##' @param x org object as character vector.
##' @return the timestamps of an orgfile as a list.
extract_timestamps_total <-
    function(x) {
        x %>%
            stringr::str_match(
                paste0(
                    "^.*CLOCK:\\ \\[[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]",
                    "{2}\\ [[:alpha:]]{2}\\ [[:digit:]]{2}:[[:digit:]]{2}\\]",
                    "--\\[[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",
                    "\\ [[:alpha:]]{2}\\ [[:digit:]]{2}:[[:digit:]]{2}\\]")) %>%
                        stringr::str_replace_all("(CLOCK:\\ \\[)|\\[|\\]", "") %>%
                        stringr::str_trim() %>%
                        stringr::str_split("--")
    }
