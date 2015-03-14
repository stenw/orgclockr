##' Extract deadlines of headings in an org file.
##'
##' This function extracts the timestamps in lines that match the
##' \code{pattern}. According to \code{org-deadline-string} these
##' lines should start with "DEADLINE:". The returned value is in the
##' format \code{\%Y-\%m-\%d}, so hours and minutes will be omitted.
##'
##' Org mode uses the standard ISO notation for dates and times as it
##' is defined in ISO 8601. The format is set in
##' \code{org-time-stamp-custom-formats}. If this variable is
##' modified, the extraction is likely to fail.
##' @param x org object as character vector.
##' @param pattern a regular expression to match the deadlines set in
##' the org file.
##' @return the deadlines of an org file as a character vector.
##' @export extract_deadlines
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_deadlines()
##' ##  [1] NA           NA           NA           NA           NA
##' ##  [6] "2014-01-04" NA           NA           NA           NA
##' ## [11] NA           NA
extract_deadlines <-
    function(x, pattern = "^\\ {0,}DEADLINE:\\ +<[[:digit:]].*>$") {
        split_file <- split_orgfile(x)
        pat <- pattern
        deadlines <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    stringr::str_match(pat) %>%
                    stringr::str_trim() %>%
                    na.omit() %>%
                    as.character()
            })
        sapply(deadlines, function(x)
            ## replace character(0) with NAs
            if (length(x) == 0L && is.character(x))
                {x <- NA} else {
                    stringr::str_replace_all(x, "(DEADLINE:\\ <)|<|>", "") %>%
                        stringr::str_trim()
                }) %>%
                    as.Date(format = "%Y-%m-%d") %>%
                    format("%Y-%m-%d") %>%
                    return()
    }
