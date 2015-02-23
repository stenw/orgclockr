context('Test the sum of difftimes as the time spent on a task.')

test_that('Sum of diffs is a measure for the time spent.', {
    expect_equal(extract_time_spent(orgfile[93:104]), 334)
    expect_equal(length(extract_time_spent(orgfile)),
                 length(extract_headlines(orgfile)))
    ## time spent vector should consist of positive values only
    expect_equal(extract_time_spent(orgfile) %>% sign() %>% sum(), 7)
})
