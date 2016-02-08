#'@title Validate and convert time objects to function with pageviews functions
#'@description \code{pageview_timestamps} converts \code{\link{Date}} and \code{\link{POSIXlt}} and ct
#'objects to work nicely with the \code{start} and \code{end} parameters in pageviews functions.
#'
#'@param timestamps a vector of character, Date, POSIXlt or POSIXct objects.
#'
#'@param first whether to, if \code{timestamps} is of date objects, assume the
#'first hour in a day (TRUE) or the last (FALSE). TRUE by default.
#'
#'@return a character vector containing timestamps that can be used with \code{\link{article_pageviews}} et al.
#'
#'@seealso \code{\link{article_pageviews}} and \code{\link{project_pageviews}}, where you
#'can make use of this function.
#'
#'@examples
#'# Using a Date
#'pageview_timestamps(Sys.Date())
#'
#'# Using a POSIXct object
#'pageview_timestamps(Sys.time())
#'
#'# Validate a character string
#'pageview_timestamps("2016020800")
#'
#'@export
pageview_timestamps <- function(timestamps = Sys.Date(), first = TRUE) {
  template <- "%Y%m%d"

  if("character" %in% class(timestamps)){
    timestamps <- strptime(timestamps, paste0(template, "%H"))
    if(anyNA(timestamps)) {
      stop(paste0("'timestamps' must be of the format ", template, "%H"))
    }
  }

  if("Date" %in% class(timestamps)){
    template <- ifelse(first == TRUE, paste0(template, "00"), paste0(template, "23"))
  } else if(any(class(timestamps) %in% c("POSIXlt", "POSIXct"))) {
    template <- paste0(template, "%H")
  } else {
    stop("'timestamps' must be of type Date, POSIXlt or POSIXct")
  }

  return(format(timestamps, template))

}
