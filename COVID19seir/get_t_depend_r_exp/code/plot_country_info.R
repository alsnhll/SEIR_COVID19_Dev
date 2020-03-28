if(!require(httr)){install.packages("httr", repos="http://cran.rstudio.com/", dependencies=TRUE)}
if(!require(RCurl)){install.packages("RCurl", repos="http://cran.rstudio.com/", dependencies=TRUE)}
if(!require(plotly)){install.packages("plotly", repos="http://cran.rstudio.com/", dependencies=TRUE)}
if(!require(dplyr)){install.packages("dplyr", repos="http://cran.rstudio.com/", dependencies=TRUE)}
library(httr)
library(RCurl)
library(plotly)
library(dplyr)

#' Script to obtain and visualize COVID-19 data
#'
#' @description A script to generate and visualize databases relevant to 
#' COVID-19. Data of confirmed cases, deceased, and recovered is obtained from 
#' the Johns Hopkins University Center for Systems Science and Engineering 
#' COVID-19 repository, which is updated daily and matches what is shown on
#' their popular mapping site.
#' @param countries vector of countries to analyze
#' @param dates named vector of begin and end dates in format M.D.YY
#' @param y_scale log or linear scale
#' @param count_by data to use: daily, active, cumulative, deceased, recovered
#' @param growth_country country to estimate growth rate

# Select parameters ------------------------------------------------------------
countries <- c("US", "Spain", "Italy", "Iran")
dates <- c(begin="3.1.20", end="3.27.20")
y_scale <- "log"
count_by <- "deceased"
growth_country <- "Iran"

# Load global data from JHU CSSEGIS --------------------------------------------
keep_info <- c("Province.State", "Country.Region", "Lat", "Long")
d_full <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
r_full <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
cc_full <- read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

# Clean data -------------------------------------------------------------------
clean_data <- function(db, keep_info) {
  db <- data.frame(lapply(db, as.character), stringsAsFactors=FALSE)
  colnames(db) <- sapply(colnames(db), function(x) gsub("X", "", x))
  db[,(length(keep_info) + 1):ncol(db)] <- 
    lapply(db[, (length(keep_info) + 1):ncol(db)], as.numeric)
  return(db)
}
d_clean <- clean_data(db=d_full, keep_info=keep_info)
r_clean <- clean_data(db=r_full, keep_info=keep_info)
cc_clean <- clean_data(db=cc_full, keep_info=keep_info)

# Subset to countries of interest ----------------------------------------------
subset_data <- function(db, countries) {
  db <- db[which(db$Country.Region %in% countries),]
  db <- db[order(db$Country.Region),]
  return(db)
}
d_subset <- subset_data(db=d_clean, countries=countries)
r_subset <- subset_data(db=r_clean, countries=countries)
cc_subset <- subset_data(db=cc_clean, countries=countries)

# Sum cases by country and get rid of Province.State column --------------------
sum_by_country <- function(db, countries) {
  for (country in countries) {
    for (col_dex in (length(keep_info) + 1):ncol(db)) {
      db[min(which(db$Country.Region == country)), col_dex] <- 
        sum(db[which(db$Country.Region == country), col_dex])
    }
  }
  db <- db[!duplicated(db$Country.Region),]
  db$Province.State <- NULL
  return(db)
}
d_sum <- sum_by_country(db=d_subset, countries=countries)
r_sum <- sum_by_country(db=r_subset, countries=countries)
cc_sum <- sum_by_country(db=cc_subset, countries=countries)
keep_info <- c("Country.Region", "Lat", "Long")

# Create active infections database --------------------------------------------
active_infections <- function(cc, r, d) {
  # Apply correction if each db does not have same # dates ---------------------
  if (ncol(cc) != ncol(r) | ncol(r) != ncol(d)) {
    min_col <- min(min(ncol(cc), ncol(r)), ncol(d))
    cc <- cc[,1:min_col]
    r <- r[,1:min_col]
    d <- d[,1:min_col]
  }
  # Apply correction to "r" database date format -------------------------------
  colnames(r) <- sapply(colnames(r), function(x) gsub("2020", "20", x))
  
  for (date in colnames(cc)[(length(keep_info) + 1):length(colnames(cc))]) {
    cc[,date] <- cc[,date] - r[,date] - d[,date]
  }
  return(cc)
}
ai <- active_infections(cc=cc_sum, r=r_sum, d=d_sum)

# Create daily infections database ---------------------------------------------
daily_infections <- function(cc) {
  for (col_dex in (length(keep_info) + 2):ncol(cc)) {
    cc[,(col_dex - 1)] <- cc[,col_dex] - cc[,(col_dex - 1)]
  }
  colnames(cc)[(length(keep_info) + 1):ncol(cc)] <- 
    c(colnames(cc)[(length(keep_info) + 2):ncol(cc)], "delete")
  cc$delete <- NULL
  return(cc)
}
di <- daily_infections(cc=cc_sum)

# Calculate daily growth rate --------------------------------------------------
calc_growth <- function(db) {
  for (col_dex in (length(keep_info) + 2):ncol(db)) {
    db[,(col_dex - 1)] <- (db[,col_dex] - db[,(col_dex - 1)]) / db[,(col_dex - 1)]
  }
  colnames(db)[(length(keep_info) + 1):ncol(db)] <- c(colnames(db)[(length(keep_info) + 2):ncol(db)], "delete")
  db$delete <- NULL
  
  # Get rid of ill-defined entries ---------------------------------------------
  db[is.na(db)] <- NA
  db <- do.call(data.frame,lapply(db, function(x) replace(x, is.infinite(x), NA)))
  db[is.na(db)] <- 0
  colnames(db) <- sapply(colnames(db), function(x) gsub("X", "", x))
  
  return(db)
}
ai_g <- calc_growth(ai)
di_g <- calc_growth(di)
d_g <- calc_growth(d_sum)
r_g <- calc_growth(r_sum)
cc_g <- calc_growth(cc_sum)

# Subset to dates of "dates" vector --------------------------------------------
subset_dates <- function(db, dates) {
  b_col <- which(colnames(db) == dates["begin"])
  e_col <- which(colnames(db) == dates["end"])
  db <- db[,c(which(colnames(db) %in% keep_info), b_col:e_col)]
  return(db)
}

# Final databases
ai_final <- subset_dates(ai, dates=dates)
di_final <- subset_dates(di, dates=dates)
d_final <- subset_dates(d_sum, dates=dates)
r_final <- subset_dates(r_sum, dates=dates)
cc_final <- subset_dates(cc_sum, dates=dates)

# Final growth rates
ai_g_final <- subset_dates(ai_g, dates=dates)
di_g_final <- subset_dates(di_g, dates=dates)
d_g_final <- subset_dates(d_g, dates=dates)
r_g_final <- subset_dates(r_g, dates=dates)
cc_g_final <- subset_dates(cc_g, dates=dates)

# Initial visualization prep of data -------------------------------------------
if (count_by == "daily") {
  to_plot <- di_final
} else if (count_by == "active") {
  to_plot <- ai_final
} else if (count_by == "cumulative") {
  to_plot <- cc_final
} else if (count_by == "deceased") {
  to_plot <- d_final
} else if (count_by == "recovered") {
  to_plot <- r_final
}
if (y_scale == "log") {
  to_plot[,(length(keep_info) + 1):ncol(to_plot)] <- 
    sapply(to_plot[,(length(keep_info) + 1):ncol(to_plot)], function (x) ifelse(x == 0, NA, log(x)))
}

# Reorganize data for plotting -------------------------------------------------
reorganize_to_plot <- function(to_plot) {
  to_plot <- to_plot[order(to_plot$Country.Region),]
  reorg_to_plot <- NA
  for (country_dex in 1:length(unique(to_plot$Country.Region))) {
    new_vec <- as.numeric(unlist(to_plot[country_dex,(length(keep_info) + 1):ncol(to_plot)]))
    if (country_dex == 1) {
      reorg_to_plot <- new_vec
    } else {
      reorg_to_plot <- cbind(reorg_to_plot, new_vec)
    }
  }
  reorg_to_plot <- data.frame(reorg_to_plot)
  reorg_to_plot <- data.frame(lapply(reorg_to_plot, as.numeric), stringsAsFactors=FALSE)
  reorg_to_plot <- 
    cbind(colnames(to_plot)[(length(keep_info) + 1):ncol(to_plot)], reorg_to_plot)
  colnames(reorg_to_plot) <- c("Date", sort(as.character(unique(to_plot$Country.Region))))
  return(reorg_to_plot)
}
reorg_to_plot <- reorganize_to_plot(to_plot=to_plot)

# Plot using plotly ------------------------------------------------------------
plot_cases <- function(reorg_to_plot, growth=F) {
  reorg_to_plot$Date <- factor(reorg_to_plot$Date, levels = reorg_to_plot[["Date"]])
  clrs <- hcl.colors(length(countries))
  countries <- sort(countries)
  for (country_dex in 1:length(countries)) {
    c_name <- countries[country_dex]
    if (country_dex == 1) {
      if (y_scale == "log") {
        basic_info_plt <- plot_ly(reorg_to_plot, x=~Date, y=reorg_to_plot[,c_name], 
                                  type='scatter', name=c_name, mode="markers",
                                  marker=list(size = 10, color=clrs[country_dex]))
      } else {
        basic_info_plt <- plot_ly(reorg_to_plot, x=~Date, y=reorg_to_plot[,c_name], 
                                  type='bar', name=c_name, marker=list(color=clrs[country_dex]))
      }
    } else {
      if (y_scale == "log") {
        basic_info_plt <- basic_info_plt %>% add_trace(y=reorg_to_plot[,c_name],
                                                       name=c_name,
                                                       marker=list(size=10, color=clrs[country_dex]))
      } else {
        basic_info_plt <- basic_info_plt %>% add_trace(y=reorg_to_plot[,c_name],
                                                       name=c_name, marker=list(color=clrs[country_dex]))
      }
    }
  }
  if (growth) {
    y_name <- paste0("daily growth ", count_by, " cases (", y_scale, ")")
  } else {
    y_name <- paste0(count_by, " cases (", y_scale, ")")
  }
  basic_info_plt <- basic_info_plt %>% layout(xaxis = list(title="Date", tickangle = -45),
                                              yaxis = list(title=y_name),
                                              margin = list(b = 100),
                                              barmode = 'group')
  return(basic_info_plt)
}
basic_info_plot <- plot_cases(reorg_to_plot=reorg_to_plot)

# Plot daily growth rate -------------------------------------------------------
if (count_by == "daily") {
  to_plot <- di_g_final
} else if (count_by == "active") {
  to_plot <- ai_g_final
} else if (count_by == "cumulative") {
  to_plot <- cc_g_final
} else if (count_by == "deceased") {
  to_plot <- d_g_final
} else if (count_by == "recovered") {
  to_plot <- r_g_final
}
reorg_to_plot <- reorganize_to_plot(to_plot=to_plot)
growth_plot <- plot_cases(reorg_to_plot=reorg_to_plot, growth=T)

# Plot growth plot and basic info plot -----------------------------------------
basic_info_plot
growth_plot

# Initial plot for exponential growth rate fit for country of choice -----------
if (count_by == "daily") {
  to_plot <- di_final
} else if (count_by == "active") {
  to_plot <- ai_final
} else if (count_by == "cumulative") {
  to_plot <- cc_final
} else if (count_by == "deceased") {
  to_plot <- d_final
} else if (count_by == "recovered") {
  to_plot <- r_final
}
to_plot <- to_plot[which(to_plot$Country.Region == growth_country),]
reorg_to_plot <- reorganize_to_plot(to_plot)

# Plot exponential growth fit for country of choice ----------------------------
plot_growth_fit <- function(reorg_to_plot, growth_country) {
  # Initialize -----------------------------------------------------------------
  reorg_to_plot$Date <- factor(reorg_to_plot$Date, levels = reorg_to_plot[["Date"]])
  clrs <- hcl.colors(1)
  c_name <- growth_country
  
  # Fitting process ------------------------------------------------------------
  y_t <- reorg_to_plot[,growth_country]
  db <- data.frame(cbind(1:length(y_t), log(y_t)))
  db <- do.call(data.frame,lapply(db, function(x) replace(x, is.infinite(x), NA)))
  db[is.na(db)] <- 0
  colnames(db) <- c("t", "y_t")
  lm_res <- lm(y_t ~ t, data=db)
  best_r <- lm_res$coefficients[2]
  best_intercept <- lm_res$coefficients[1]
  fit_y <- vector()
  for (i in 1:length(y_t)) {
    fit_y <- c(fit_y, best_intercept + (1 * best_r * i))
  }
  reorg_to_plot$fit_y <- exp(fit_y)
  
  # Plotting -------------------------------------------------------------------
  growth_fit_plt <- plot_ly(reorg_to_plot, x=~Date, y=reorg_to_plot[,c_name], type='bar', 
              name=c_name, marker=list(color=clrs[1]))
  growth_fit_plt <- growth_fit_plt %>% 
    add_trace(x=~Date, y=~fit_y, type='scatter', mode='lines+markers', name='exp. fit', 
               line=list(color='red'), marker = list(color = 'red'))
  y_name <- paste0(count_by, " cases (linear)")
  growth_fit_plt <- growth_fit_plt %>% 
    layout(xaxis = list(title="Date", tickangle = -45),
           yaxis = list(title=y_name, side="left"),
           margin = list(b = 100),
           barmode = 'group')
  growth_fit_plt
  return(growth_fit_plt)
}
growth_fit_plt <- plot_growth_fit(reorg_to_plot=reorg_to_plot, growth_country=growth_country)

# Plot exponential fit ---------------------------------------------------------
growth_fit_plt

# Calculate curve flattening index for countries of choice ---------------------

