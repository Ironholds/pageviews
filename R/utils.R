#'@importFrom httr stop_for_status GET user_agent content
pv_query <- function(params, ...){
  url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/", params)
  result <- httr::GET(url, httr::user_agent("pageviews API client library - https://github.com/Ironholds/pageviews"))

  # Check response success
  if(httr:status_code(result) == 404){
    stop("The pageview data available does not cover the range you requested")
  } else {
    httr::stop_for_status(result)
  }

  return(httr::content(result))
}
