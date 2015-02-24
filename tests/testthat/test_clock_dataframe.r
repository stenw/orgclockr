context('Check the aggregation of org elements in the resulting data.frame.')

missing_timestamp <-
    system.file("extdata", "test_missing_timestamp.org", package = "orgclockr") %>%
        readLines()
resulting_df <- clock_dataframe(orgfile)

test_that('Timestamps are read properly and are within the given
period per headline.', {
          expect_equal(clock_dataframe(missing_timestamp) %>% nrow(),
                       1)
          expect_equal(resulting_df %>% sapply(typeof) %>% as.character(),
                      c("character", "character", "character", "integer",
                        "character", "double", "double", "character",
                        "character", "double"))
          expect_equal(resulting_df %>% length(), 10)
          expect_equal(resulting_df %>% nrow(), 12)
                        })
