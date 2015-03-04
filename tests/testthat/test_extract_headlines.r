context('Check the extracting of headlines.')

test_that('Tags and keywords should be removed properly.', {
    ## several tags
    expect_equal("* Blog                                                                     :blog:write:PC:" %>%
                 extract_headlines(),
                 "Blog")
    ## uppercase single letter
    expect_equal("*** TODO R - class() or typeof()" %>%
                 extract_headlines(),
                 "R - class() or typeof()")
    ## several uppercase words
    expect_equal("*** TODO DSUR - SAGE – Discovering Statistics Using R" %>%
                 extract_headlines(),
                 "DSUR - SAGE – Discovering Statistics Using R")
})
