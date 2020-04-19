library(tseries)

#' Script to fit time dependent growth rate of an exponential function
#'
#' @description script to fit the time dependent growth rate of an exponential
#' function using local linear regression, with and without an intercept, as 
#' well as fitting the best exp. function to the data. Current data is the
#' NYT U.S. state data of number of confirmed cases, number of deceased, and
#' daily incidence of confirmed cases.
#' @return plot of growth rate, exponential fit on log scale, and exponential 
#' fit on linear scale for the following three data sources: number of confirmed
#' cases, number of deceased, and daily incidence of confirmed cases.

# Load NYT data -- get confirmed and deceased data -----------------------------
nyt_full <- read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
confirmed_deceased_data <- function(db, c_or_d) {
  cases_date_ls <- list()
  index <- 1
  # Loop through each date -----------------------------------------------------
  for (date in as.character(unique(db$date))) {
    cases_date <- nyt_full[which(db$date == date),]
    to_add <- data.frame(matrix(rep(0, times=(length(unique(db$state)) + 1)), 
                                ncol=(length(unique(db$state)) + 1), nrow=1))
    colnames(to_add) <- c(as.character(unique(db$state)), "total")
    # Sum all county cases of state --------------------------------------------
    for (state in as.character(unique(cases_date$state))) {
      if (c_or_d == "c") {
        to_add[1,state] <- sum(cases_date$cases[which(cases_date$state == state)])
      } else {
        to_add[1,state] <-sum(cases_date$deaths[which(cases_date$state == state)])
      }
    }
    # Add information to final database ----------------------------------------
    to_add$total[1] <- rowSums(to_add)
    cases_date_ls[[index]] <- to_add
    index <- index + 1
  }
  cases_date <- do.call(rbind, cases_date_ls)
  cases_date$exp <- exp(0.4 * seq(1:nrow(cases_date))) # simple exp. example ---
  cases_date$date <- as.character(unique(db$date))
  return(cases_date)
}
nyt_confirmed <- confirmed_deceased_data(db=nyt_full, c_or_d="c")
nyt_deceased <- confirmed_deceased_data(db=nyt_full, c_or_d="d")

# Get daily incidence data -----------------------------------------------------
get_daily_incidence <- function(db) {
  save_date <- db$date
  db$date <- NULL
  db <- rbind(rep(0, ncol(db)), db)
  for (row_num in 2:nrow(db)) {
    db[(row_num - 1),] <- db[row_num,] - db[(row_num - 1),]
  }
  db <- db[1:(nrow(db) - 1),]
  db$date <- save_date[1:nrow(db)]
  print(save_date[length(save_date)])
  return(db)
}
nyt_daily <- get_daily_incidence(db=nyt_confirmed)
nyt_daily$exp <- exp(0.4 * seq(1:nrow(nyt_daily))) # simple exp. example -------

# Plot r(t) for cumulative, daily, deceased data (fit by state) ----------------
plot_r_t <- function(db, type, start, window) {
  # Hawaii and Wyoming manual removal from "deceased" plot (since there are none yet)
  if (type == "deceased") {
    db$Hawaii <- NULL
    db$Wyoming <- NULL
  }
  # Remove Virgin Islands and Northern Mariana Islands -------------------------
  db$`Virgin Islands` <- NULL
  db$`Northern Mariana Islands` <- NULL
  
  # Prepare layout of pdf ------------------------------------------------------
  pdf(file=paste0("~/SEIR_COVID19_Dev/COVID19seir/get_t_depend_r_exp/plots/", 
                  type,
                  "_start_", start, 
                  "_window_", window, ".pdf"),width=8, height=16)
  par(mfrow=c(6,3))
  
  # Loop through each state and solve for instantaneous growth rate ------------
  states <- colnames(db)[1:(ncol(db) - 1)]
  for (state in states) {
    # Define beginning of time series ------------------------------------------
    first_occ_nonzero <- min(which(db[,state] != 0)) # 1st occur of nonzero elem
    t <- db[(max(1, (first_occ_nonzero - 1)):nrow(db)), state]
    t <- as.numeric(t)
    t <- t[min(which(t >= start)):length(t)]
    
    # Solve for r(t) using intercept (red line) --------------------------------
    rs <- list()
    rs_dex <- 1
    intercepts <- list()
    i_dex <- 1
    for (i in (1 + window):(length(t) - window)) {
      t_local <- log(t[(i - window):(i + window)])
      t_local <- sapply(t_local, function (x) ifelse(is.infinite(x), 0, x)) # infinity correction
      local_exp_fit <- data.frame(cbind(c((i - window):(i + window)), t_local))
      colnames(local_exp_fit) <- c("xs", "ys")
      local_exp_fit_res <- lm(ys ~ xs, data=local_exp_fit)
      rs[[rs_dex]] <- local_exp_fit_res$coefficients[2]
      rs_dex <- rs_dex + 1
      intercepts[[i_dex]] <- local_exp_fit_res$coefficients[1]
      i_dex <- i_dex + 1
    }
    rs <- do.call(rbind, rs)
    rs <- data.frame(rs)
    first_last <- data.frame(matrix(c(NA), ncol=1, nrow=1)) # Add NA for times we did not solve for
    colnames(first_last) <- c("xs")
    rs <- rbind(first_last, rs)
    rs <- rbind(rs, first_last)
    
    # Plot "using-intercept" r(t) (red line) -----------------------------------
    plot(rs$xs, main=state, ylab="r(t)", xlab="t (days)", ylim=c(0, 1), col="red", type="l")
    intercepts <- do.call(rbind, intercepts)
    intercepts <- data.frame(intercepts)
    first_last <- data.frame(matrix(c(NA), ncol=1, nrow=1)) # Add NA for times we did not solve for
    colnames(first_last) <- c("X.Intercept.")
    intercepts <- rbind(first_last, intercepts)
    intercepts <- rbind(intercepts, first_last)
    
    # Solve for r(t) without using intercept, and plot (green line) ------------
    rs_2 <- list()
    rs_2_dex <- 1
    for (i in (1 + window):(length(t) - window)) {
      t_local <- log(t[(i - window):(i + window)])
      t_local <- sapply(t_local, function (x) ifelse(is.infinite(x), 0, x)) # infinity correction
      local_exp_fit <- data.frame(cbind(c((i - window):(i + window)), t_local))
      colnames(local_exp_fit) <- c("xs", "ys")
      local_exp_fit_res <- lm(ys ~ 0 + xs, data=local_exp_fit)
      rs_2[[rs_2_dex]] <- local_exp_fit_res$coefficients[1]
      rs_2_dex <- rs_2_dex + 1
    }
    rs_2 <- do.call(rbind, rs_2)
    rs_2 <- data.frame(rs_2)
    first_last <- data.frame(matrix(c(NA), ncol=1, nrow=1)) # Add NA for times we did not solve for
    colnames(first_last) <- c("xs")
    rs_2 <- rbind(first_last, rs_2)
    rs_2 <- rbind(rs_2, first_last)
    lines(rs_2$xs, col="green")
    
    # Solve best exp. fit entire t-series (i.e. constant r(t)) (blue line) -----
    exp_fit <- data.frame(cbind(1:length(t), log(t)))
    colnames(exp_fit) <- c("xs", "ys")
    exp_fit$ys <- ifelse(is.infinite(exp_fit$ys), 0, exp_fit$ys) # infinity correction
    lm_res <- lm(ys ~ xs, data=exp_fit)
    best_r <- lm_res$coefficients[2]
    best_intercept <- lm_res$coefficients[1]
    abline(h=best_r, col="blue")
    
    # Plot the time-dependent-r exp. fit on log scale (middle panel) -----------
    ys <- vector()
    ys2 <- vector()
    ys3 <- vector()
    for (i in 2:(length(t) - 1)) { # Loop through all time to get data point using each of three methods
      ys <- c(ys, 1 * exp(rs[i, 1] * i))
      ys2 <- c(ys2, 1 * exp(best_r * i))
      ys3 <- c(ys3, 1 * exp(rs_2[i, 1] * i))
    }
    ys <- c(rep(NA, window), ys, rep(NA, window)) # Add NA for times we did not solve for
    ys2 <- c(rep(NA, window), ys2, rep(NA, window))
    ys3 <- c(rep(NA, window), ys3, rep(NA, window))
    plot(log(t), main=state, ylab=paste0("log(# ", type," cases)"), 
         xlab="t (days)", ylim=c(0, 10), pch=10, cex=0.5)
    lines(intercepts + log(ys), col="red")
    lines(best_intercept + log(ys2), col="blue")
    lines(log(ys3), col="green")
    
    # Plot the time-dependent-r exp. fit on linear scale (right panel) ---------
    ys <- vector()
    ys2 <- vector()
    ys3 <- vector()
    for (i in 2:(length(t) - 1)) {  # Loop through all time to get data point using each of three methods
      ys <- c(ys, rs[i, 1] * i)
      ys2 <- c(ys2, best_r * i)
      ys3 <- c(ys3, rs_2[i, 1] * i)
    }
    ys <- c(rep(NA, window), ys, rep(NA, window)) # Add NA for times we did not solve for
    ys2 <- c(rep(NA, window), ys2, rep(NA, window))
    ys3 <- c(rep(NA, window), ys3, rep(NA, window))
    plot(t, main=state, ylab=paste("# ", type, " cases"), xlab="t (days)", pch=20, cex=0.5)
    lines(exp(intercepts + ys), col="red")
    lines(exp(best_intercept + ys2), col="blue")
    lines(exp(ys3), col="green")
  }
  dev.off()
}
plot_r_t(db=nyt_confirmed, type="confirmed", start=1, window=1)
plot_r_t(db=nyt_daily, type="daily", start=1, window=1)
plot_r_t(db=nyt_deceased, type="deceased", start=1, window=1)


