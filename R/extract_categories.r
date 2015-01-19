##' Extract the categories of an org file.
##'
##' This function is used to extract the categories matching the
##' regexp pattern.
##' @param orgfile org object as character vector.
##' @return the categories of an orgfile as a character matrix.
##' @export extract_tags
##' @seealso \code{extract_headlines}, \code{extract_timestamps}
##' and \code{extract_tags} for extractions of other org elements.
extract_categories <-
    function(orgfile) {
        str_match(orgfile, "^[ :]+CATEGORY:\\ \\w+") %>%
            str_replace("^:CATEGORY:\\ ", "") %>%
            na.omit()
    }
