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
##' ## [1] "TODO TaskTen"
##' @seealso \code{extract_time_spent}, \code{extract_efforts},
##' \code{extract_todostates}, \code{extract_tags},
##' \code{extract_timestamps}, \code{extract_levels} and
##' \code{extract_categories} to extract other org elements.
extract_headlines <-
    function(x) {
        tags          <- extract_tags(x, inherit = FALSE) %>% na.omit() %>%
            paste0(collapse = "|")
        keywords      <- extract_todostates(x) %>% na.omit() %>%
            paste0(collapse = "|")
        replace_this  <- c(tags, keywords, ":") %>% paste0(collapse = "|")
        headlines     <-
            x %>%
                extract_raw_headlines()
        lapply(seq_along(headlines), function(i) {
            headlines[i] %>%
                stringr::str_replace("^\\*{1, }\\ ", "") %>%
                stringr::str_replace_all(replace_this, "") %>%
                stringr::str_trim()
        }) %>%
            unlist() %>%
            na.omit() %>%
            as.character()
    }
