## ------------------------------------------------------------------------
library(pageviews)
str(article_pageviews(project = "de.wikipedia", article = "R_(Programmiersprache)"
  , start = as.Date('2015-11-01'), end = as.Date("2015-11-02")
  , user_type = c("user", "bot"), platform = c("desktop", "mobile-web")))

## ------------------------------------------------------------------------
str(project_pageviews())

## ------------------------------------------------------------------------
str(top_articles())

