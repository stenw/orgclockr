context('Check the splitting of an org file and the aggregation of categories per node.')

test_that('More categories for one node are concatenated to one object.', {
    expect_equal(c(split_orgfile(orgfile)[[1]],
                   ":CATEGORY: OneMore",
                   ":CATEGORY: AndAnotherOne") %>%
                       extract_categories() %>% length(), 1)
    expect_equal(length(extract_efforts(orgfile)),
                 length(extract_headlines(orgfile)))
})
