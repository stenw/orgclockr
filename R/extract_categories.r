##' Extract the categories of an org file.
##'
##' This function is used to extract the categories matching the
##' regexp pattern.
##' @param x org object as character vector.
##' @return the categories of an orgfile as a character vector.
##' @export extract_categories
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_categories() %>%
##' first()
##' ## [1] "CategoryOne"
##' @seealso \code{extract_time_spent}, \code{extract_efforts},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags} and
##' \code{extract_levels} to extract other org elements.
extract_categories <-
    function(x) {
        split_file <- split_orgfile(x)
        categories <-
            lapply(seq_along(split_file), function(i) {
                split_file[[i]] %>%
                    unlist() %>%
                    extract_categories_total() %>%
                    ## should concatenate the categories per node. not
                    ## sure if more categories per node are reasonable
                    ## in orgmode though.
                    paste0(collapse = " ")
            })
        categories[which(categories == "")] <- NA
        unlist(categories) %>%
        return()
    }
