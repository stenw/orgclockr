##' Extract the headlines of an org file.
##'
##' This function is used to extract the headlines matching the regexp
##' pattern.
##' @param x org object as character vector.
##' @return the headlines of an orgfile as a character matrix.
##' @export extract_headlines
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_headlines() %>%
##' last()
##' ## [1] "TaskTen"
##' @seealso \code{extract_intervals}, \code{extract_days_on_task},
##' \code{extract_time_spent}, \code{extract_efforts},
##' \code{extract_todostates}, \code{extract_tags},
##' \code{extract_timestamps}, \code{extract_levels} and
##' \code{extract_categories} to extract other org elements.
extract_headlines <-
    function(x) {
        headlines <-
            x %>%
                extract_raw_headlines()
        lapply(seq_along(headlines), function(i) {
            headlines[i] %>%
                stringr::str_replace("^\\*{1,}\\s{1,}", "") %>%
                ## remove tags
                stringr::str_replace_all("(:[[:alnum:]]{1,}){1,}:$", "") %>%
                ## remove keywords
                stringr::str_replace_all(
                    stringr::perl(
                        "^\\b[[:upper:]]{2,}((\\b)|(_[[:upper:]]+\\b))(?!(\\s{2,}|\\t|$))"),
                    "") %>%
                        stringr::str_trim()
        }) %>%
            unlist() %>%
            na.omit() %>%
            as.character()
    }
