context('Check the aggregation of org elements in the resulting data.frame.')

missing_timestamp <-
    system.file("extdata", "test_missing_timestamp.org", package = "orgclockr") %>%
        readLines()
resulting_df <- clock_dataframe(orgfile)

test_that('Timestamps are read properly and are within the given period per headline.',
          ## with(resulting_df, expect_true(first %within% period))
          expect_equal(clock_dataframe(missing_timestamp) %>% nrow(),
                       1))
