##' Extract all efforts set in an org file.
##'
##' The regular expression to match the effort property in an org file
##' will be passed from \code{extract_efforts}.
##' @param x org object as character vector.
##' @param pattern regular expression to match the effort property set
##' by \code{org-effort-property}.
##' @return all efforts set in an org file.
extract_efforts_total <-
    function(x, pattern) {
        matching_pattern <-
            stringr::str_c(pattern, "\\ +[[:digit:]].*")
        stringr::str_match(x, matching_pattern) %>%
            stringr::str_trim() %>%
            stringr::str_replace(pattern, "") %>%
            stringr::str_trim() %>%
            na.omit() %>%
            as.character()
    }
