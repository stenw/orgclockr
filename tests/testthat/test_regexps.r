context("Regexp patterns to get elements from an org file.")

orgfile <-
    ## load raw datafile
    system.file("extdata", "sample.org", package = "orgclockr") %>%
        ## read in samplefile
        readLines()

test_that('Pattern for headlines.', {
     expect_output(extract_headlines(orgfile[1]), "HeadingOne")
     expect_equal(class(extract_headlines(orgfile[1])), "character")
     expect_true(is.vector(extract_headlines(orgfile)))
     ## check filtering todo keywords AND tags with
     ## str_replace_all()
     expect_equal(
         "** TODO TaskSix                                        :TagTwo:" %>%
             extract_headlines(), "TaskSix")
})

test_that('Pattern for timestamps.', {
    expect_equal(class(extract_timestamps(orgfile)), "list")
    expect_equal(extract_timestamps(orgfile[82])[[1]] %>% length(), 2)
})

test_that('Pattern for categories.', {
    expect_output(extract_categories(orgfile)[1], "CategoryOne")
    expect_true(is.vector(extract_categories(orgfile)))
     })

test_that('Pattern for levels.', {
    expect_equal("** Headline test " %>% extract_levels(), 2)
    expect_true(is.vector(extract_levels(orgfile)))
})

test_that('Pattern for todostates.', {
     expect_equal(c("* TEST_THAT test", "** TODO Test TESTING") %>%
                     extract_todostates(),
                  c("TEST_THAT", "TODO"))
     expect_output(extract_todostates("** TODO test", c("HABIT", "TODO")),
                   "TODO")
     ## regarding NAs
     expect_equal(extract_todostates(c("* TODO test", "** Test Headline"),
                  c("DONE", "TODO")) %>% length(), 2)
     expect_true(is.vector(extract_todostates(orgfile)))
     })

test_that('Test if the length of output of extract_ functions is correct. ', {
    expect_equal(length(c(
        extract_categories(orgfile),
        extract_headlines(orgfile),
        extract_levels(orgfile),
        extract_tags(orgfile),
        extract_todostates(orgfile)
        )), 5 * nrow(orgfile))
})