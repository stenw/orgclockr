##' Extract levels of headings in an org file.
##'
##' Get the \code{org-level} in an org file by counting the prepending
##' heading star characters.
##' @param x org object as character vector.
##' @return the levels of headlines in an orgfile as an integer
##' vector.
##' @export extract_levels
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_levels()
##' ##  [1] 1 2 2 2 1 2 2 1 2 2 3 2
extract_levels <-
    function(x) {
        x %>%
            extract_raw_headlines() %>%
            stringr::str_extract("^\\*{1,}\\ ") %>%
            na.omit() %>%
            stringr::str_trim() %>%
            stringr::str_length()
    }
