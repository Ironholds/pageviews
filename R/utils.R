#'@importFrom httr stop_for_status GET user_agent content status_code
pv_query <- function(params, reformat, ...){
  # Run multiple queries, return as list of results
  data_list <- lapply(as.list(params), pv_query_single, reformat = reformat)

  if(reformat == TRUE){ # collapses all results into one dataframe
    return(do.call(rbind, data_list))
  }
  return(data_list)
}

pv_query_single <- function(params, reformat, ...){
  url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/", params)

  result <- httr::GET(url, httr::user_agent("pageviews API client library - https://github.com  /Ironholds/pageviews"))
  # Check response success
  if(httr::status_code(result) == 404){
    stop(httr::content(result, type = "application/json")$detail)
  } else {
    httr::stop_for_status(result)
  }

  data <- httr::content(result)

  if(reformat){
    data <- reformat_data(data)
  }

  return(data)
}

reformat_data <- function(data){
  data <- data$items

  if("articles" %in% names(data[[1]])){ # Handle Top Articles formatting
    result <- as.data.frame(do.call(rbind, data[[1]]$articles),
                        stringsAsFactors = FALSE)
    data[[1]]$articles <- NULL
    meta <- do.call(cbind, data[[1]])
    data <- cbind(meta, result)

    if(all(data$day == "all")){
      data["granularity"] <- "month"
      data$day <- 1
    } else {
      data["granularity"] <- "day"
    }

    data$date <- as.POSIXct(paste(data$year, data$month, data$day, sep = "-"))
    data$rank <- as.numeric(data$rank)
  } else {
    data <- as.data.frame(do.call(rbind, data), stringsAsFactors = FALSE)
    data$date <- as.POSIXct(as.character(data$timestamp), format = "%Y%m%d%H", tz = "GMT")
  }


  # Convert all variables to appropriate classes & formats.
  data$views <- as.numeric(data$views)
  data$language <- gsub("(.*)\\.(.*)", "\\1", data$project)
  data$project <- gsub("(.*)\\.(.*)", "\\2", data$project)
  data$access <- as.character(data$access)
  data$granularity <- as.character(data$granularity)

  # Agent and article only contained in some queries
  if(!is.null(data$agent)){
    data$agent <- as.character(data$agent)
  }
  if(!is.null(data$article)){
    data$article <- as.character(data$article)
  }


  # Set Consistent Column Order
  col_order <- c("project", "language", "article", "access",
                "agent", "granularity", "date", "rank", "views")
  col_order <- col_order[col_order %in% names(data)]
  data <- data[, col_order]

  return(data)
}
