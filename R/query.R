pageviews <- function(api, project, article, platform, user_type,
                      granularity, start, end, reformat, ...){
  
  # Handle timestamps
  start <- pageview_timestamps(start)
  if(is.null(end)){
   if (granularity == "monthly"){
      end <- as.Date(start, format = "%Y%m%d%H") + 31
    }else{
      end <- start
    }
  }
  end <- pageview_timestamps(end) 
  

  
  platform[platform == "all"] <- "all-access"
  user_type[user_type == "all"] <- "all-agents"
  
  # Construct parameters
  parameters <- expand.grid("pageviews/", api,
                            project, platform, user_type,
                            article, granularity, start, end)
  parameters <- apply(parameters, 1, paste, collapse = "/")
  parameters <- gsub("\\/{1,}", "/", parameters)
  
  # Run
  data <- pv_query(parameters, reformat, ...)
  return(data)
}


#'@title Retrieve Pageview Data for an Article
#'@description retrieves the pageview data for a particular article on a project, within
#'a provided time-range.
#'
#'@param project the name of the project, structured as \code{[language_code].[project]}
#'(see the default).
#'
#'@param article the article(s) you want to retrieve data for. Ideally features underscores in the title
#'instead of spaces, but happily converts if you forget to do this.
#'
#'@param platform The platform the pageviews came from; One or more of "all", "desktop", "mobile-web" and
#'"mobile-app". Set to "all" by default.
#'
#'@param user_type the type of users. One or more of "all", "user", "spider" or "bot". "all" by default.
#'
#'@param start the start \code{YYYYMMDDHH} of the range you want to cover. This can be
#'easily grabbed from R date/time objects using \code{\link{pageview_timestamps}}.
#'
#'@param end the end \code{YYYYMMDDHH} of the range you want to cover. NULL by default, meaning
#'that it returns 1 day of data.
#'
#'@param reformat Whether to reformat the results as a \code{\link{data.frame}} or not. TRUE by default.
#'
#'@param granularity the granularity of data to return; "daily" or "monthly", depending on
#' whether pageview data should reflect trends in days or months.
  
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso \code{\link{top_articles}} for the top articles per project in a given date range,
#'and \code{\link{project_pageviews}} for per-project pageviews.
#'
#'@examples
#'# Basic example
#'r_pageviews <- article_pageviews()
#'
#'# Modify the article
#'obama_pageviews <- article_pageviews(article = "Barack_Obama")
#'
#'@export
article_pageviews <- function(project = "en.wikipedia", article = "R (programming language)",
                              platform = "all", user_type = "all",
                              start = "2015100100", end = NULL, reformat = TRUE, 
                              granularity="daily",...){

  article <- gsub(x = article, pattern = " ", replacement = "_", fixed = TRUE)
  article <- curl::curl_escape(article)

  data <- pageviews("per-article", project, article, platform, user_type,
                    granularity, start, end, reformat, ...)
  return(data)
}

#'@title Retrieve Data on Top Articles
#'@description \code{top_articles} grabs data on the top articles for a project
#'in a given time period, and for a particular platform.
#'
#'@param project the name of the project, structured as \code{[language_code].[project]}
#'(see the default).
#'
#'@param platform The platform the pageviews came from; one or more of  "all", "desktop", "mobile-web" and
#'"mobile-app". Set to "all" by default.
#'
#'@param start The date the articles were "top" in. 2015 by default.
#'
#'@param granularity the granularity of data to return; "daily" or "monthly", depending on
#' whether top articles should reflect trends in day or month of the \code{start} date.
#'
#'@param reformat Whether to reformat the results as a \code{\link{data.frame}} or not. TRUE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso \code{\link{article_pageviews}} for per-article pageviews and \code{\link{project_pageviews}} for
#'per-project pageviews.
#'
#'@examples
#'# Basic example
#'enwiki_top_articles <- top_articles()
#'
#'# Use a narrower platform
#'enwiki_mobile_top <- top_articles(platform = "mobile-web")
#'
#'@importFrom jsonlite fromJSON
#'@export
top_articles <- function(project = "en.wikipedia", platform = "all",
                        start = as.Date("2015-10-01"),
                        granularity = "daily", reformat = TRUE, ...) {

  platform[platform == "all"] <- "all-access"

  parameters <- expand.grid("pageviews/top",
                            project, ifelse(platform == "all", "all-access", platform),
                            format(start, "%Y"), format(start, "%m"),
                            ifelse(granularity == "daily", format(start, "%d"), "all-days"))

  parameters <- apply(parameters, 1, paste, collapse = "/")
  results <- pv_query(parameters, reformat, ...)

  return(results)
}

#'@title Retrieve Per-Project Pageview Counts
#'
#'@description Retrieve pageview counts for a particular project.
#'
#'@param project the name of the project, structured as \code{[language_code].[project]}
#'(see the default).
#'
#'@param platform The platform the pageviews came from; one or more of  "all", "desktop", "mobile-web" and
#'"mobile-app". Set to "all" by default.
#'
#'@param user_type the type of users. one or more of  "all", "user", "spider" or "bot". "all" by default.
#'
#'@param granularity the granularity of data to return; do you want hourly or daily counts? Set
#'to "daily" by default.
#'
#'@param start the start \code{YYYYMMDDHH} of the range you want to cover. This can be
#'easily grabbed from R date/time objects using \code{\link{pageview_timestamps}}
#'
#'@param end the end \code{YYYYMMDDHH} of the range you want to cover. NULL by default, meaning
#'that it returns 1 day/hour of data (depending on the value passed to \code{granularity}).
#'
#'@param reformat Whether to reformat the results as a \code{\link{data.frame}} or not. TRUE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@examples
#'# Basic call
#'enwiki_1_october_pageviews <- project_pageviews()
#'
#'# Break it down to hourly
#'enwiki_hourly <- project_pageviews(granularity = "hourly", end = "2015100123")
#'
#'@seealso \code{\link{old_pageviews}}, for 2008-2016 data, \code{\link{top_articles}} for the top articles
#'per project in a given date range, and \code{\link{article_pageviews}}
#'for per-article pageviews.
#'
#'@export
project_pageviews <- function(project = "en.wikipedia", platform = "all", user_type = "all",
                              granularity = "daily", start = "2015100100", end = NULL,
                              reformat = TRUE, ...){
  data <- pageviews("aggregate", project, article = "", platform, user_type,
                      granularity, start, end, reformat)
  return(data)
}

#'@title Retrieve Legacy Pageview Counts
#'
#'@description This retrieves per-project pageview counts from January 2008
#'to July 2016. These counts are calculated using the 'legacy' (read: old)
#'model, which overcounts due to its inclusion of web-crawlers and
#'similar automata.
#'
#'@param project the name of the project, structured as \code{[language_code].[project]}
#'(see the default).
#'
#'@param platform The platform the pageviews came from; one or more of  "all", "desktop" or "mobile".
#'Set to "all" by default.
#'
#'@param granularity the granularity of data to return; do you want hourly, daily or monthly counts? Set
#'to "daily" by default.
#'
#'@param start the start \code{YYYYMMDDHH} of the range you want to cover. This can be
#'easily grabbed from R date/time objects using \code{\link{pageview_timestamps}}
#'
#'@param end the end \code{YYYYMMDDHH} of the range you want to cover. NULL by default, meaning
#'that it returns 1 day/hour of data (depending on the value passed to \code{granularity}).
#'
#'@param reformat Whether to reformat the results as a \code{\link{data.frame}} or not. TRUE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@examples
#'# Basic call
#'enwiki_2013_2015_old <- old_pageviews()
#'
#'# Break it down to hourly
#'old_enwiki_hourly <- old_pageviews(granularity = "hourly", end = "2013110100")
#'
#'@seealso \code{\link{top_articles}} for the top articles per project in a given date range,
#'\code{\link{project_pageviews}} for per-project pageviews under the new definition,
#'and \code{\link{article_pageviews}} for per-article pageviews.
#'
#'@export
old_pageviews <- function(project = "en.wikipedia", platform = "all",
                          granularity = "daily", start = "2013100100",
                          end = "2015100100", reformat = TRUE, ...){
  
  # Handle timestamps
  start <- pageview_timestamps(start)
  if(is.null(end)){
    if (granularity == "monthly"){
      end <- as.Date(start, format = "%Y%m%d%H") + 31
    }else{
      end <- start
    }
  }
  end <- pageview_timestamps(end) 

  
  # Clean up platform choice
  platform[platform == "all"] <- "all-sites"
  platform[platform == "desktop"] <- "desktop-site"
  platform[platform == "mobile"] <- "mobile-site"
  
  parameters <- expand.grid("legacy/pagecounts/aggregate",
                            project, platform, granularity, start,
                            end)
  parameters <- apply(parameters, 1, paste, collapse = "/")
  
  results <- pv_query(parameters, reformat, old = TRUE, ...)
  
  return(results)
}
