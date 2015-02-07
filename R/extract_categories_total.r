##' Extract the categories of an org file.
##'
##' @param x org object as character vector.
##' @return the categories of an orgfile as a character vector.
extract_categories_total <-
    function(x) {
        stringr::str_match(x, "^[ :]+CATEGORY:\\ \\w+") %>%
            stringr::str_replace("^:CATEGORY:\\ ", "") %>%
            na.omit() %>%
            as.character()
    }
