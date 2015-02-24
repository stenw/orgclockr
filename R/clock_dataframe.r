##' Aggregate the org elements of an org file in a data.frame.
##'
##' This function creates a \code{data.frame} object that aggregates
##' the org elements of each heading in a row. In addition,
##' \code{First} and \code{Last} reveal the first and the last
##' timestamp stored for each heading. The period between \code{First}
##' and \code{Last} is returned in the \code{Period} column. The unit
##' of time in the columns Effort, Time_Spent and Period is controlled
##' by the \code{units} parameter. Efforts make use of the time units
##' \code{"mins"} and \code{"hours"} only, because the format H:MM is
##' hard-coded for the effort property in org-mode. If the
##' \code{inherit_tags} parameter is set to \code{TRUE}, the tags from
##' level one headings will be inherited. Use \code{pattern_states}
##' and \code{pattern_efforts} to control the regular expressions to
##' match the \code{org-todo-keywords} and the effort entries in an
##' org file.
##' @param x org object as character vector.
##' @param units unit of time used in the columns Effort, Time_Spent
##' and Period.
##' @param inherit_tags logical; if \code{TRUE} (default), inherit
##' tags from level one headlines.
##' @param pattern_states a regular expression to match the
##' \code{org-todo-keywords} in the headings.
##' @param pattern_efforts a regular expression to match the effort
##' entries.
##' @return data.frame object.
##' @export clock_dataframe
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' clock_dataframe(units = "hours") %>%
##' head(3)
##' ##      Category           Tag   Headline Level State Effort Time_Spent
##' ## 1 CategoryOne        TagOne HeadingOne     1  <NA>   <NA>       0.00
##' ## 2        <NA> TagOne TagTwo    TaskOne     2  TODO   <NA>       0.00
##' ## 3        <NA>        TagOne    TaskTwo     2  TODO   0.33       0.35
##' ##                 First                Last Period
##' ## 1                <NA>                <NA>     NA
##' ## 2                <NA>                <NA>     NA
##' ## 3 2014-10-08 Mi 13:34 2014-10-10 Fr 17:05  51.52
##' @seealso \code{extract_time_spent}, \code{extract_categories},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags},
##' \code{extract_efforts} and \code{extract_levels} to extract
##' particular org elements.
clock_dataframe <-
    function(x,
             units           = "mins",
             inherit_tags    = TRUE,
             pattern_states  = "\\b[[:upper:]]+((\\b)|(_[[:upper:]]+\\b))",
             pattern_efforts = "^[ :]+Effort:") {
        Category   <- extract_categories(x)
        Tag        <- extract_tags(x, inherit = inherit_tags)
        Headline   <- extract_headlines(x)
        Level      <- extract_levels(x)
        State      <- extract_todostates(x, pattern = pattern_states)
        Effort     <- extract_efforts(x, pattern = pattern_efforts, units =
                                          units)
        Last       <- lapply(extract_timestamps(x), first) %>%
            unlist()
        First      <- lapply(extract_timestamps(x), last) %>%
            unlist()
        Period     <- difftime(lubridate::ymd_hm(Last),
                               lubridate::ymd_hm(First), units = units) %>%
                                   as.numeric() %>%
                                   ## If there is only one pair of
                                   ## timestamps, the last timestamp
                                   ## is actually in the First
                                   ## variable. Circumventing this by
                                   ## returning the absolute value
                                   abs() %>%
                                   round(2)
        Time_Spent <- extract_time_spent(x, units = units)
        cbind.data.frame(Category,
                         Tag,
                         Headline,
                         Level,
                         State,
                         Effort,
                         Time_Spent,
                         First,
                         Last,
                         Period)
    }
