if(!require(httr)){install.packages("httr", repos="http://cran.rstudio.com/", dependencies=TRUE)}
if(!require(RCurl)){install.packages("RCurl", repos="http://cran.rstudio.com/", dependencies=TRUE)}
library(httr)
library(RCurl)

#===================================================================================
# Script to obtain growth rate of number of confirmed cases per day
#
# - A file that obtains the growth rate, for a given number of days, and for a vector of countries.
#   We may also subdivide into the region / state level if available.
# - Data is obtained from the github repository, https://github.com/benflips/nCovForecast, who gives
#   a description of the sources used to create the .csv's of the time series of confirmed cases.
# - User gives vector of countries they want the growth rate for (e.g. c("US", "Cyprus")). To get a
#   list of the available countries, use the command, unique(cc$Country.Region)
# - User gives the dates that they want in c(begin_date, end_date) vector format using date format:
#   "M.D.YY"
# - User tells whether we want to estimate growth on cumulative prevalence, daily incidence, or
#   number of active infections (number in the I box), one of: "cumulative", "daily", "active"
#===================================================================================
countries <- c("Iran", "Italy", "Spain") 
dates <- c(begin="3.8.20", end="3.18.20") # Note: current code gets data from begin + 1 to end
by_country <- TRUE
count_by <- "active"

#===================================================================================
# - First, we load in the data directly from:
#   https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/
#   which is updated daily.
# - There are two levels of the data: Country.Region (larger) and Province.State (smaller). Select
#   whether we want the information by "Country.Region" or by "Province.State."
# - If by_country == TRUE, reduce to Country.Region instead of keeping Province.State subdivision
# - Calculate growth rate via https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/
#   which is simply (infections_t - infections_(t-1)) / infections_(t-1)
# - Get rid of entries that are ill-defined (i.e. we divide by 0 or divide 0/0 when calculating growth rate)
# - Output dataframe of the growth rate by Country.Region, or by Province.State
#===================================================================================
deaths <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
d <- read.csv(text = deaths)
d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
colnames(d) <- sapply(colnames(d), function(x) gsub("X", "", x))
d <- d[which(d$Country.Region %in% countries),]

recovered <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
r <- read.csv(text = recovered)
r <- data.frame(lapply(r, as.character), stringsAsFactors=FALSE)
colnames(r) <- sapply(colnames(r), function(x) gsub("X", "", x))
r <- r[which(r$Country.Region %in% countries),]

infections <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
cc <- read.csv(text = infections)
cc <- data.frame(lapply(cc, as.character), stringsAsFactors=FALSE)
colnames(cc) <- sapply(colnames(cc), function(x) gsub("X", "", x))
cc <- cc[which(cc$Country.Region %in% countries),]

# Assumes format of data does not change
if (count_by == "active" | count_by == "daily") {
  # Create "active inspections" by subtracting cc - d - r
  for (date in colnames(cc)[5:length(colnames(cc))]) {
    cc[,date] <- as.numeric(cc[,date]) - as.numeric(r[,date]) - as.numeric(d[,date])
  }
  if (count_by == "daily") {
    for (col_dex in 6:ncol(cc)) {
      diff <- cc[,col_dex] - cc[,(col_dex - 1)]
      cc[,(col_dex - 1)] <- ifelse(diff < 0, 0, diff)
    }
    colnames(cc)[5:ncol(cc)] <- c(colnames(cc)[6:ncol(cc)], "delete")
    cc$delete <- NULL
  }
}

b_col <- which(colnames(cc) == dates["begin"])
e_col <- which(colnames(cc) == dates["end"])
keep_info <- c("Province.State", "Country.Region", "Lat", "Long")
cc <- cc[,c(which(colnames(cc) %in% keep_info), b_col:e_col)]
b_col <- length(keep_info) + 1
e_col <- ncol(cc)
if (by_country) { 
  for (country in countries) {
    for (col_dex in b_col:e_col) {
      cc[min(which(cc$Country.Region == country)), col_dex] <- 
        sum(as.numeric(cc[which(cc$Country.Region == country), col_dex]))
    }
  }
  cc <- cc[!duplicated(cc$Country.Region),]
  cc$Province.State <- NULL
  b_col <- b_col - 1
  e_col <- e_col - 1
}
cc[,b_col:e_col] <- lapply(cc[, b_col:e_col], as.numeric)
for (col_dex in (b_col + 1):e_col) {
  cc[,(col_dex - 1)] <- (cc[,col_dex] - cc[,(col_dex - 1)]) / cc[,(col_dex - 1)]
}
colnames(cc)[b_col:e_col] <- c(colnames(cc)[(b_col + 1):e_col], "delete")
cc$delete <- NULL
cc[is.na(cc)] <- NA
cc <- do.call(data.frame,lapply(cc, function(x) replace(x, is.infinite(x), NA)))

#===================================================================================
# Testing: use cc to get the same tab as in Ben Phillips' shiny app
# NOTE: this visualization has not been rigorously tested, this was just done as a cross check of
#       Ben Phillips' method
#===================================================================================
cc[is.na(cc)] <- 0
if (by_country) {
  clrs <- hcl.colors(length(unique(cc$Country.Region)))
  barplot(as.matrix(cc[,4:ncol(cc)]), main="Growth rate", legend=unique(cc$Country.Region),
          xlab="Date", col=clrs, beside=TRUE)
} else {
  clrs <- hcl.colors(length(unique(cc$Province.State)))
  barplot(as.matrix(cc[,5:ncol(cc)]), main="Growth rate",
          xlab="Date", col=clrs, beside=TRUE)
}
