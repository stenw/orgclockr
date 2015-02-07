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
##' @export extract_efforts
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_efforts() %>%
##' ##  [1] NA     NA     "0:20" NA     NA     NA     "1:00" NA     "0:30" "0:25"
##' ## [11] "4:00" "0:25"
##' @seealso \code{extract_categories}, \code{extract_todostates},
##' \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_tags} and \code{extract_levels} to extract other org
##' elements.
extract_efforts <-
    function(x, pattern = "^[ :]+Effort:") {
        split_file <- split_orgfile(x)
        pat <- pattern
        efforts <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    extract_efforts_total(pattern = pat) %>%
                    paste0(collapse = " ")
            })
        efforts[which(efforts == "")] <- NA
        unlist(efforts) %>%
            return()
    }
