context('Check the extracting of headlines.')

test_that('Extract uppercase headlines as headlines and not as todostates.', {
    expect_equal("** INBOX     :tag:" %>% extract_todostates() %>% na.omit() %>%
                     paste0(collapse = "|"),
                 "")
})
