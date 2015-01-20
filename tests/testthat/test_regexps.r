context("Regexp patterns to get elements from an org file.")

orgfile <-
    ## load raw datafile
    system.file("extdata", "sample.org", package = "orgclockr") %>%
        ## read in samplefile
        readLines()

test_that('Pattern for headlines.', {
     expect_output(extract_headlines(orgfile[1]),
                   "* HeadingOne                                                                  :TagOne:")
     expect_equal(class(extract_headlines(orgfile[1])), "matrix")
})

test_that('Pattern for timestamps.', {
    expect_equal(class(extract_timestamps(orgfile)), "list")
    expect_equal(extract_timestamps(orgfile[82])[[1]] %>% length(), 2)
})

test_that('Pattern for categories.', {
    expect_output(extract_categories(orgfile)[1], "CategoryOne")
     })

test_that('Pattern for levels.', {
    expect_equal("** Headline test " %>%
                     extract_levels(), 3)
})