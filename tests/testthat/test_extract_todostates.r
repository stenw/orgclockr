context('Check the extracting of todostates.')

test_that('Do not extract uppercase headlines as todostates.', {
    expect_true("** INBOX     :tag:" %>% extract_todostates() %>% is.na())
    expect_equal("** INBOX test     :tag:" %>% extract_todostates(), "INBOX")
    expect_true("** INBOX" %>% extract_todostates() %>% is.na())
})
