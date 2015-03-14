##' Extract the tags of an org file.
##'
##' This function is used to extract the tags matching the regexp
##' pattern.
##'
##' @details If there is more than one tag for a headline, they will
##' be bundled in one character object separated by one space
##' character. The headlines without any tag will give a \code{NA}
##' value, effectively resulting in a vector of as many elements as
##' the number of headlines in the org file.
##'
##' If the \code{inherit} parameter is set to TRUE (default), the
##' function will make use of tag inheritance given by the org-mode
##' variable \code{org-use-tag-inheritance}. So far, the inheritance
##' only works for level one tags.
##' @param x org object as character vector.
##' @param inherit logical; if \code{TRUE} (default), inherit tags
##' from level one headlines.
##' @return the tags of an orgfile as a character vector.
##' @export extract_tags
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' extract_tags(inherit = FALSE)
##' ##  [1] "TagOne"                 "TagTwo"                 NA
##' ##  [4] NA                       "TagThree"               "TagTwo"
##' ##  [7] "TagTwo"                 "TagOne TagThree TagTwo" NA
##' ## [10] NA                       NA                       NA
extract_tags <-
    function(x, inherit = TRUE) {
        tags <-
            x %>%
                extract_raw_headlines() %>%
                na.omit() %>%
                stringr::str_extract("(:[[:alnum:]]{1,}){1,}:$") %>%
                stringr::word() %>%
                stringr::str_replace_all(":", " ") %>%
                stringr::str_trim()
        levels <-
            x %>% extract_levels()
        if (inherit) {
            sapply(split(tags,
                         cumsum(seq_along(levels) %in% which(levels == 1))),
                   function(x) stringr::str_c(x[1], x, sep = " ") %>%
                       stringr::str_replace_all("NA", "") %>%
                       unlist() %>%
                       stringr::str_split("\ ") %>%
                       sapply(function(x) unique(x) %>%
                                  unlist() %>%
                                  stringr::str_c(collapse = " ") %>%
                                  stringr::str_trim() %>%
                                  stringr::str_replace("^$", NA))
                   ) %>%
                       unlist() %>%
                       as.character() %>%
                       return()
        } else {
            return(tags)
        }
    }
