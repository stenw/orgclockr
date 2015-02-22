context('Check the aggregation of timestamps per node.')

test_that('Patterns match the timestamps.', {
    expect_equal(length(extract_timestamps(orgfile)),
                 length(extract_headlines(orgfile)))
 })
