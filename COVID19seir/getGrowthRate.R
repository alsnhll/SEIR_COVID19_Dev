if(!require(httr)){install.packages("httr", repos="http://cran.rstudio.com/", dependencies=TRUE)}
if(!require(RCurl)){install.packages("RCurl", repos="http://cran.rstudio.com/", dependencies=TRUE)}
library(httr)
library(RCurl)

#===================================================================================
# Script to obtain growth rate of number of confirmed cases per day
#
# - Data is on cases, deaths, and recoveries is obtained from the Johns Hopkins University Center for Systems Science and Engineering COVID-19 repository, which is updated daily and matches what is shown on their popular mapping site
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

# ALH: I'm confused about what's going on here? Is this overriding the real case counts with calculations? This is not a good idea. New dataframes should be created for the active or daily cases and for the growth rates. And can we add some comments here about what is being done in each code chunk?

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
# Visualize data with the following graphs
#===================================================================================

#===================================================================================
# Plot cases over time for countries of choice
#===================================================================================

# Like main page of Ben Philips app, but using Plotly instead for graphs, and only having a single plot. We can make an option to change the y axis scale from linear ot log, don't need two separate lots for this
# Use chooses which type of case count to view (cumulative, daily, active cases or deaths or recovered)
# Multiple countries can be plotted ... same list that user chose at begining of file

#===================================================================================
# Calculate daily growth rates for countries of choice
#===================================================================================

# Same output as Growth Rate tab of Ben Phillips’ app, but done with Plotly

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

#===================================================================================
# Calculate curve flattening index for countries of choice
#===================================================================================

# Same output as Growth Rate tab of Ben Phillips’ app, but done with Plotly

#===================================================================================
# Estimate growth rate for country of choice
#===================================================================================

# This is related to what is done in the forecasting from Ben Philip's all (shown on his main page, overlayed on the data, but with slightly better methods)

# Now user must choose only a single country
# User chooses which time range to use to calculate growth rate r (start and end times) (right now default is for 10 days but user can choose a different range)
# r is calculated using Poisson regression on daily case counts  (incidence) over the time period chosen
# Plot cases over time overlayed with curve fit (note that although it’s statistically better to fit to incidence, the cumulative and active case counts will also growth with the same exponential rate, as should deaths and recovered, so the user could choose which of these to display, though for now we can stick with plotting all cases or incidence)

