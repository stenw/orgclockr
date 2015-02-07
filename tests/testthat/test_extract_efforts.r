context('Check the aggregation of effort times per node.')

test_that('Regexp patterns match the effort property.', {
    expect_equal(extract_efforts(orgfile)[3], "0:20")
    expect_equal(length(extract_efforts(orgfile)),
                 length(extract_headlines(orgfile)))
 })
