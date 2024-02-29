# pageviews
An API client library for Wikimedia traffic data.

__Author:__ Os Keyes<br/>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br/>
__Status:__ Stable

![Build Status](https://travis-ci.org/Ironholds/pageviews.svg?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ores)](https://cran.r-project.org/package=ores
) ![downloads](http://cranlogs.r-pkg.org/badges/grand-total/pageviews)

`pageviews` provides data from the new Wikimedia RESTful API for pageview data. It allows you to retrieve per-article, per-project, and top-1000 pageview data covering a wide range of times and with filtering around the user's class and platform.

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/Ironholds/pageviews/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

## Installation

For the stable release:

    install.packages("pageviews")

For the developer release:

    devtools::install_github("ironholds/pageviews")
