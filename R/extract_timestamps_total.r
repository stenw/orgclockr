##' Extract the timestamps of an org file.
##'
##' This function is used to extract the timestamps matching the
##' regexp pattern.
##' @param x org object as character vector.
##' @return the timestamps of an orgfile as a list.
##' @export extract_timestamps_total
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_timestamps_total()[c(55:56)]
##' ## [[1]]
##' ## [1] "2015-01-01 Do 17:40" "2015-01-01 Do 18:14"
##' ##
##' ## [[2]]
##' ## [1] "2014-12-31 Mi 10:51" "2014-12-31 Mi 11:14"
##' ##
##' @seealso \code{extract_efforts_total}, \code{extract_todostates},
##' \code{extract_headlines}, \code{extract_tags},
##' \code{extract_levels} and \code{extract_categories} to extract
##' other org elements.
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
