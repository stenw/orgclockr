context('Check the aggregation of org elements in the resulting data.frame.')

missing_timestamp <-
    system.file("extdata", "test_missing_timestamp.org", package = "orgclockr") %>%
        readLines()
resulting_df <- org_elements_df(orgfile)

test_that('Timestamps are read properly and are within the given
period per headline.', {
    expect_equal(org_elements_df(missing_timestamp) %>% nrow(),
                 1)
    expect_equal(resulting_df %>% sapply(typeof) %>% as.character(),
                 c("character", "character", "character", "integer", "character",
                   "character", "double", "double", "character", "character",
                   "double", "integer", "double"))
    expect_equal(resulting_df %>% length(), 13)
    expect_equal(resulting_df %>% nrow(), 12)
})
