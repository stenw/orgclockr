##' Extract the categories of an org file.
##'
##' This function is used to extract the categories matching the
##' regexp pattern.
##' @param x org object as character vector.
##' @return the categories of an orgfile as a character matrix.
##' @export extract_tags
##' @seealso \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_tags} and \code{extract_levels} for extractions of
##' other org elements.
extract_categories <-
    function(x) {
        stringr::str_match(x, "^[ :]+CATEGORY:\\ \\w+") %>%
            stringr::str_replace("^:CATEGORY:\\ ", "") %>%
            na.omit()
    }
