context('Check whether the tag inheritance works.')

test_that('Tags are inherited.', {
    expect_equal(extract_tags(orgfile[5]), "TagTwo")
    expect_true("* TODO Testheader        " %>%
                    extract_tags() %>%
                    is.na())
    expect_equal(extract_tags(orgfile, inherit = TRUE)[2],
                 "TagOne TagTwo")
    expect_equal(extract_tags(orgfile) %>% length(),
                 extract_headlines(orgfile) %>% length())
})
