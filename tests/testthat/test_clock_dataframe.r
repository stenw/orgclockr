context('Check the aggregation of org elements in the resulting data.frame.')

resulting_df <- clock_dataframe(orgfile)

## test_that('Timestamps are read properly and are within the given period per headline.',
##           with(resulting_df, expect_true(first %within% period)))
