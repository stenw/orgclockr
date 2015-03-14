##' Aggregate the org elements of an org file.
##'
##' This function creates a \code{dplyr::data_frame} object that
##' aggregates the org elements in each heading. The columnnames
##' 'Headline' and 'Tag' are self-explanatory. The 'State' column
##' returns the \code{org-todo-keywords} set for the given heading.
##' The 'Level' returns a value according to the output of
##' \code{org-outline-level}. In addition to the information that can
##' be found in an org heading, the categories and deadlines, which
##' are stored as properties in a dedicated drawer, are returned per
##' heading. The unit of time in the column 'Effort' is controlled by
##' the \code{units} parameter. Efforts make use of the time units
##' \code{"mins"} and \code{"hours"} only, because the format H:MM is
##' hard-coded for the effort property in Org mode. If the
##' \code{inherit_tags} parameter is set to \code{TRUE}, the tags from
##' level one headings will be inherited. Use \code{pattern_states}
##' and \code{pattern_efforts} to control the regular expressions to
##' match the \code{org-todo-keywords} and the effort entries in an
##' org file.
##'
##' Org mode uses the standard ISO notation for dates and times as it
##' is defined in ISO 8601. The format is set in
##' \code{org-time-stamp-custom-formats}. If this variable is
##' modified, the extraction of 'Deadline' is likely to fail.
##' @param x org object as character vector.
##' @param units unit of time used in the column Effort
##' @param inherit_tags logical; if \code{TRUE} (default), inherit
##' tags from level one headlines.
##' @param pattern_states a regular expression to match the
##' \code{org-todo-keywords} in the headings.
##' @param pattern_efforts a regular expression to match the effort
##' entries.
##' @return a \code{dplyr::data_frame} object.
##' @export org_elements_df
##' @examples
##' system.file("extdata", "sample.org", package = "orgclockr") %>%
##' readLines() %>%
##' org_elements_df(units = "hours") %>%
##' head(3)
##' ## Source: local data frame [3 x 7]
##' ##
##' ##     Headline    Category           Tag Level State Deadline Effort
##' ## 1 HeadingOne CategoryOne        TagOne     1    NA       NA     NA
##' ## 2    TaskOne          NA TagOne TagTwo     2  TODO       NA     NA
##' ## 3    TaskTwo          NA        TagOne     2  TODO       NA   0.33
##' @seealso \code{extract_intervals}, \code{extract_deadlines},
##' \code{extract_time_spent}, \code{extract_categories},
##' \code{extract_todostates}, \code{extract_headlines},
##' \code{extract_timestamps}, \code{extract_tags},
##' \code{extract_efforts} and \code{extract_levels} to extract
##' particular org elements. Use \code{org_clock_df} to extract
##' various clocking information.
org_elements_df <-
    function(x,
             units           = "mins",
             inherit_tags    = TRUE,
             pattern_states  = "\\b[[:upper:]]{2,}((\\b)|(_[[:upper:]]+\\b))(?!(\\s{2,}|\\t|$))",
             pattern_efforts = "^[ :]+Effort:") {
        Category   <- extract_categories(x)
        Tag        <- extract_tags(x, inherit = inherit_tags)
        Headline   <- extract_headlines(x)
        Level      <- extract_levels(x)
        State      <- extract_todostates(x, pattern = pattern_states)
        Effort     <- extract_efforts(x, pattern = pattern_efforts, units =
                                          units)
        Deadline   <- extract_deadlines(x)
        dplyr::data_frame(Headline,
                          Category,
                          Tag,
                          Level,
                          State,
                          Deadline,
                          Effort)
    }
