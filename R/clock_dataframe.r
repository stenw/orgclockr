clock_dataframe <-
    function(x) {
        categories    <- extract_categories(x)
        tags          <- extract_tags(x)
        headlines     <- extract_headlines(x)
        levels        <- extract_levels(x)
        ## timestamps <- extract_timestamps(x)
        todostates    <- extract_todostates(x)
        efforts       <- extract_efforts(x)
        cbind(categories,
              tags,
              headlines,
              levels,
              ## timestamps,
              todostates,
              efforts)
    }
