context("Regexp patterns to get elements from an org file.")

orgfile <-
    ## load raw datafile
    system.file("extdata", "sample.org", package = "orgclockr") %>%
        ## read in samplefile
        readLines()

test_that('Pattern for headlines.', {
     expect_output(cut_headlines(orgfile[1]),
                   "* HeadingOne                                                                  :TagOne:")
})