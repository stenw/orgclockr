##' Extract the headlines of an org file.
##'
##' This function is used to extract the headlines matching the regexp
##' pattern.
##' @param x org object as character vector.
##' @return the headlines of an orgfile as a character matrix.
##' @export extract_headlines
##' @seealso \code{extract_tags}, \code{extract_timestamps},
##' \code{extract_levels} and \code{extract_categories} for
##' extractions of other org elements.
extract_headlines <-
    function(x) {
        tags <-
            extract_tags(x)
        headlines <-
            x %>%
                extract_raw_headlines()
        lapply(seq_along(headlines), function(i) {
            headlines[i] %>%
                stringr::str_replace(tags, "") %>%
                min() %>%
                stringr::str_trim() %>%
                stringr::str_replace("^\\*{1, }\\ ", "")
        }) %>%
            unlist() %>%
            na.omit() %>%
            as.character()
    }
