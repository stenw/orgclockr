##' Aggregate the org elements of an org file in a data.frame.
##'
##' This function creates a \code{data.frame} object that aggregates
##' the org elements of each heading in a row. In addition,
##' \code{first} and \code{last} reveal the first and the last
##' timestamp stored for each heading. The period between \code{first}
##' and \code{last} is output in the \code{period} column. The unit of
##' time in the period column is controlled by the \code{units}
##' parameter.
##' @param x org object as character vector.
##' @param units unit of time used in the period column.
##' @param inherit_tags logical; if \code{TRUE} (default), inherit
##' tags from level one headlines.
##' @return data.frame object.
##' @export clock_dataframe
##' @examples
##' @seealso \code{extract_categories}, \code{extract_todostates},
##' \code{extract_headlines}, \code{extract_timestamps},
##' \code{extract_tags}, \code{extract_efforts} and
##' \code{extract_levels} to extract particular org elements.
clock_dataframe <-
    function(x, units = "mins", inherit_tags = TRUE) {
        categories    <- extract_categories(x)
        tags          <- extract_tags(x)
        headlines     <- extract_headlines(x)
        levels        <- extract_levels(x)
        todostates    <- extract_todostates(x)
        efforts       <- extract_efforts(x)
        last          <- lapply(extract_timestamps(orgfile), first) %>%
            unlist()
        first         <- lapply(extract_timestamps(orgfile), last) %>%
            unlist()
        period        <- difftime(lubridate::ymd_hm(last),
                                  lubridate::ymd_hm(first), units = units) %>%
                                      abs() %>%
                                      round(2)
        cbind(categories,
              tags,
              headlines,
              levels,
              ## timestamps,
              todostates,
              efforts,
              first,
              last,
              period)
    }
