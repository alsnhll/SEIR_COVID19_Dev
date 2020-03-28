library(tseries)

# Arma model
state_res <- list()
state_res_dex <- 1
for (state in unique(states$Province.State)[c(1:48, 50:51)]) {
  first_occ_nonzero <- min(which(states[which(states$Province.State == state),5:ncol(states)] != 0))
  t <- states[which(states$Province.State == state), (4+first_occ_nonzero):ncol(states)]
  t <- as.numeric(t)
  arma_res <- arma(t, c(1, 1))
  state_res[[state_res_dex]] <- c(state, arma_res$coef)
  state_res_dex <- state_res_dex + 1
}
state_res <- do.call(rbind, state_res)


# Linear regression solutions
states <- read.csv("~/Desktop/US_by_state_daily_confirmed_JHU.csv")
to_add <- data.frame(matrix(NA, ncol=ncol(states), nrow=1))
colnames(to_add) <- colnames(states)
to_add$Province.State <- "US"
to_add$Country.Region <- "US"
to_add[,5:ncol(states)] <- sapply(states[5:ncol(states)], function(x) sum(as.numeric(x)))
states <- rbind(states, to_add)

pdf(file=paste0("~/COVID19seir/get_t_depend_r_exp/plots/fit_time_dependent_exponential.pdf"),width=8, height=8)
par(mfrow=c(4,4))
for (state in as.character(unique(states$Province.State))) {
  first_occ_nonzero <- min(which(states[which(states$Province.State == state),5:ncol(states)] != 0))
  t <- states[which(states$Province.State == state), (4+first_occ_nonzero):ncol(states)]
  t <- as.numeric(t)
  
  # Solve for r(t)
  rs <- list()
  rs_dex <- 1
  intercepts <- list()
  i_dex <- 1
  for (i in 2:(length(t) - 1)) {
    # svd_res <- svd(matrix(c(i - 1, i, i + 1), nrow=3, ncol=1))
    # pseudo <- svd_res$v %*% as.matrix(1/svd_res$d) %*% t(svd_res$u)
    # rs[[rs_dex]] <- pseudo %*% log(t[c(i - 1, i, i + 1)])
    # rs_dex <- rs_dex + 1
    
    db <- data.frame(cbind(c(i - 1, i, i + 1), log(t[c(i - 1, i, i + 1)])))
    colnames(db) <- c("xs", "ys")
    db_res <- lm(ys ~ xs, data=db)
    rs[[rs_dex]] <- db_res$coefficients[2]
    rs_dex <- rs_dex + 1
    intercepts[[i_dex]] <- db_res$coefficients[1]
    i_dex <- i_dex + 1
  }
  rs <- do.call(rbind, rs)
  rs <- data.frame(rs)
  first_last <- data.frame(matrix(c(NA), ncol=1, nrow=1))
  colnames(first_last) <- c("xs")
  rs <- rbind(first_last, rs)
  rs <- rbind(rs, first_last)
  plot(rs$xs, main=state, ylab="r(t)", xlab="t (days)", ylim=c(0, 1))
  
  intercepts <- do.call(rbind, intercepts)
  intercepts <- data.frame(intercepts)
  first_last <- data.frame(matrix(c(NA), ncol=1, nrow=1))
  colnames(first_last) <- c("X.Intercept.")
  intercepts <- rbind(first_last, intercepts)
  intercepts <- rbind(intercepts, first_last)
  
  # Solve for best exponential fit (no variance, r(t) is constant)
  # svd_res <- svd(matrix(1:length(t), nrow=length(t), ncol=1))
  # pseudo <- svd_res$v %*% as.matrix(1/svd_res$d) %*% t(svd_res$u)
  # best_r <- pseudo %*% log(t[1:length(t)])
  db <- data.frame(cbind(1:length(t), log(t)))
  colnames(db) <- c("xs", "ys")
  lm_res <- lm(ys ~ xs, data=db)
  best_r <- lm_res$coefficients[2]
  best_intercept <- lm_res$coefficients[1]
  abline(h=best_r, col="blue")
  
  # Graph the time dependent exponential function
  ys <- vector()
  ys2 <- vector()
  for (i in 2:(length(t) - 1)) { # For each time step
    ys <- c(ys, 1 * exp(rs[i,1] * i))
    ys2 <- c(ys2, 1 * exp(best_r * i))
  }
  ys <- c(NA, ys)
  ys <- c(ys, NA)
  ys2 <- c(NA, ys2)
  ys2 <- c(ys2, NA)
  if (state == "US") {
    plot(log(t), main=state, ylab="log(# active cases)", xlab="t (days)", ylim=c(0, 12))
  } else {
    plot(log(t), main=state, ylab="log(# active cases)", xlab="t (days)", ylim=c(0, 10))
  }
  lines(intercepts + log(ys), col="red")
  lines(best_intercept + log(ys2), col="blue")
  if (all(rs[2:(length(t) - 1), 1] < 0)) {
    print("Negative correlation exists for:")
    print(state)
  }
}
dev.off()
