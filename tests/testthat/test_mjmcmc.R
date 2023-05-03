# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-02-25

context("MJMCMC")

test_that("Testing MJMCMC algorithm", {
  # Dummy test likelihood function
  loglik.tester <- function (y, x, model, complex, params) {
    lmmod <- lm.fit(x[, model, drop = FALSE], y)
    n <- length(y)
    k <- length(lmmod$coefficients)
    rss <- sum(resid(lmmod)^2)
    aic <- n * log(rss / n) + 2 * k
    return(list(crit = aic))
  }

  data <- matrix(rnorm(600), 100)
  resm <- mjmcmc(data, loglik.tester)
  summary(resm)
  plot(resm)

  resg <- gmjmcmc(data, loglik.tester, NULL, c("log", "exp"))
  summary(resg)
  plot(resg)
  # predict(resg) TODO

  respm <- mjmcmc.parallel(2, 2, data, loglik.tester)
  summary(respm)
  plot(respm)
  # predict(resg) TODO

  respg <- gmjmcmc.parallel(2, 2, data, loglik.tester, NULL, c("log", "exp"))
  # summary(respg) TODO
  # plot(respg) TODO
  # predict(respg) TOO: First needs to run merge
})
