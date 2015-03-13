context('Check the clocking elements in the df.')

resulting_df <-
    org_clock_df(orgfile)
intervals <- extract_intervals(orgfile, units = "mins")

## TODO splitting intervals at midnight
## test_that('DaysOnTask regard intervals spanning over two days.',
##           expect_equal(resulting_df$DaysOnTask,
##                        sapply(extract_timestamps(orgfile),
##                               function(x) trunc(x, "days") %>%
##                                   unique() %>%
##                                   length()) %>%
##                                   rep(times =
##                                           sapply(extract_intervals(orgfile),
##                                                  length))))
test_that('AvgClockInterval returns right values.',
          expect_true(resulting_df %>%
                          filter(Headline == "TaskNine") %>%
                          select(AvgClockInterval) ==
                          extract_intervals(orgfile[83]))
          )
