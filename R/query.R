#'@title Retrieve Pageview Data for an Article
#'@description retrieves the pageview data for a particular article on a project, within
#'a provided time-range.
#'
#'@export
article_pageviews <- function(project = "en.wikipedia", article = "Barack Obama",
                              platform = "all", user_type = "all", granularity = "daily",
                              start = "2015100100", end = NULL, ...){
  
  if(length(article) > 1){
    return(lapply(article, article_pageviews, project = project, platform = platform,
                  user_type = user_type, granularity = granularity, start = start, end = end, ...))
  }
  # Handle timestamps
  if(is.null(end)){
    end <- start
  }
  
  # Construct parameters
  parameters <- paste("per-article", project, ifelse(platform == "all", "all-access", platform),
                      ifelse(user_type == "all", "all-agents", user_type), article, granularity,
                      start, end, sep = "/")
  
  # Run and return
  return(pv_query(parameters, ...)$items)
}

#'@title Retrieve Data on Top Articles
#'@description \code{top_articles} grabs data on the top articles for a project
#'in a given time period, and for a particular platform.
#'
#'@param project the name of the project, structured as \code{[language_code].[project]}
#'(see the default).
#'
#'@param platform The platform the pageviews came from; one of "all", "desktop", "mobile-web" and
#'"mobile-app". Set to "all" by default.
#'
#'@param year The year the articles were "top" in. "all" by default; if it is "all", month
#'and day must also be "all".
#'
#'@param month The month the articles were "top" in. "all" by default; if it is "all", day must also
#'be "all".
#'
#'@param day The day the articles were "top" in. "all" by default; this simply returns the top articles
#'for the relevant month/year combination.
#'
#'@seealso \code{\link{article_pageviews}} for per-article pageviews and \code{\link{project_pageviews}} for
#'per-project pageviews.
#'
#'@export
top_articles <- function(project = "en.wikipedia", platform = "all", year = "all",
                         month = "all", day = "all", ...) {
  
  parameters <- paste("top", project, ifelse(platform == "all", "all-access", platform),
                      ifelse(year == "all", "all-years", year), ifelse(year == "all", "all-years", year))
  return(pv_query(parameters, ...)$items)
}

#'@export
project_pageviews <- function(project = "en.wikipedia", platform = "all", user_type = "all",
                              granularity = "daily", start = "2015100100", end = NULL, reformat = TRUE,
                              ...){
  
  # Handle timestamps
  if(is.null(end)){
    end <- start
  }
  
  # Construct parameters
  parameters <- paste("aggregate", project, ifelse(platform == "all", "all-access", platform),
                      ifelse(user_type == "all", "all-agents", user_type), granularity,
                      start, end, sep = "/")
  
  # Run
  data <- pv_query(parameters, ...)$items
  
  # Reformat if necessary, return either way.
  if(reformat){
    nameset <- names(data[[1]])
    data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE), stringsAsFactors = FALSE)
    names(data) <- nameset
    data$views <- as.numeric(data$views)
    return(data)
  }
  
  return(data)
}
