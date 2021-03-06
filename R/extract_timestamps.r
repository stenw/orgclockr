##' Extract the timestamps of an org file per headline.
##'
##' This function is used to extract the timestamps per headline
##' matching a regular expression. An even number of timestamps per
##' headline is expected, so org files with running clocks will fail
##' to return the single timestamps and ignore them. Org mode uses the
##' standard ISO notation for dates and times as it is defined in ISO
##' 8601. The format is set in \code{org-time-stamp-custom-formats}.
##' If this variable is modified, the extraction is likely to fail.
##' @param x org object as character vector.
##' @return the timestamps of an orgfile as a list.
##' @export extract_timestamps
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_timestamps() %>%
##' last()
##' ##  [1] "2015-01-19 Mo 22:57" "2015-01-20 Di 00:05" "2015-01-19 Mo 21:55"
##' ##  [4] "2015-01-19 Mo 22:12" "2015-01-19 Mo 21:43" "2015-01-19 Mo 21:44"
##' ##  [7] "2015-01-19 Mo 20:40" "2015-01-19 Mo 21:42" "2015-01-19 Mo 19:25"
##' ## [10] "2015-01-19 Mo 19:41" "2015-01-19 Mo 16:11" "2015-01-19 Mo 19:01"
extract_timestamps <-
    function(x) {
        split_file <- split_orgfile(x)
        timestamps <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    extract_timestamps_total() %>%
                    unlist() %>%
                    na.omit() %>%
                    stringr::str_c()
            })
        timestamps[which(timestamps == "")] <- NA
        return(timestamps)
    }
