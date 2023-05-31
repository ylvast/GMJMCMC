#' Run multiple mjmcmc runs in parallel, merging the results before returning.
#' @param runs The number of runs to run
#' @param cores The number of cores to run on
#' @param ... Parameters to pass to mjmcmc
#' @return Merged results from multiple mjmcmc runs
#' @export
mjmcmc.parallel <- function (runs, cores = getOption("mc.cores", 2L), ...) {
  results <- mclapply.new(seq_len(runs), function (x) { mjmcmc(...) }, mc.cores = cores)
  class(results) <- "mjmcmc_parallel"
  return(results)
}


#' Run multiple gmjmcmc runs in parallel returning a list of all results.
#' @param runs The number of runs to run
#' @param cores The number of cores to run on
#' @param ... Parameters to pass to gmjmcmc
#' @return Results from multiple gmjmcmc runs
#' @export
gmjmcmc.parallel <- function (runs, cores = getOption("mc.cores", 2L), merge.options = NULL, data, loglik.pi, loglik.alpha, transforms, ...) {
  options("gmjmcmc-transformations" = transforms)
  results <- mclapply.new(seq_len(runs), function (x) {
    gmjmcmc(data = data, loglik.pi = loglik.pi, loglik.alpha = loglik.alpha, transforms = transforms, ...)
  }, mc.cores = cores)
  class(results) <- "gmjmcmc_parallel"
  merged <- merge.results(results, merge.options$populations, merge.options$complex.measure, merge.options$tol)
  return(merged)
}
