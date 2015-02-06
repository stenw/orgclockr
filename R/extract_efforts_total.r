##' Extract the efforts set in an org file.
##'
##' This function extracts the efforts matching the regexp pattern.
##' The pattern matches the property \code{Effort} according to the
##' settings in \code{org-effort-property} by default.
##' @param pattern a regular expression to match the property that is
##' being used to keep track of effort estimates.
##' @param x org object as character vector.
##' @return the efforts set in an org file as a character vector.
##' These have the format H:MM.
##' @export extract_efforts_total
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_efforts_total() %>%
##' ## [1] "0:20" "1:00" "0:30" "0:25" "4:00" "0:25"
##' @seealso \code{extract_categories_total},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags} and
##' \code{extract_levels} to extract other org elements.
extract_efforts_total <-
    function(x, pattern = "^[ :]+Effort:\\ +") {
        matching_pattern <-
            stringr::str_c(pattern, "[[:digit:]].*")
        stringr::str_match(x, matching_pattern) %>%
            stringr::str_trim() %>%
            stringr::str_replace(pattern, "") %>%
            na.omit() %>%
            as.character()
    }
