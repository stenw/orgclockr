##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param i
##' @return .. type of object the function returns
##' @export .. name of function
##' @examples
##' @references
##' @author Bernhard Proell
##' @keywords
cut_headlines <-
    function(i) {
        i %>%
            str_match("^\\*{1, }\\s.+")
    }
