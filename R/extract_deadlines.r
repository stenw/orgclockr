##' Extract deadlines of headings in an org file.
##'
##' This function extracts the timestamps in lines that match the
##' \code{pattern}. According to \code{org-deadline-string} these
##' lines should start with "DEADLINE:".
##' @param x org object as character vector.
##' @param pattern a regular expression to match the deadlines set in
##' the org file.
##' @return the deadlines of an org file as a character vector.
##' @export extract_deadlines
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_deadlines()
##' ##  [1] NA              NA              NA              NA
##' ##  [5] NA              "2014-01-04 Sa" NA              NA
##' ##  [9] NA              NA              NA              NA
##' @seealso \code{extract_intervals}, \code{extract_days_on_task},
##' \code{extract_time_spent}, \code{extract_categories},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags} and
##' \code{extract_levels} to extract other org elements.
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
                    return()
    }
