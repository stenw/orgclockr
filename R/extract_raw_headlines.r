##' Extract the raw headlines of an org file.
##'
##' @param x org object as character vector.
##' @return headlines as the whole lines.
extract_raw_headlines <-
    function(x) {
        x %>%
            stringr::str_match("^\\*{1,}\\s.+")
    }
