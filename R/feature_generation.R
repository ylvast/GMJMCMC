# Title     : Features generation for use in GMJMCMC (Genetically Modified MJMCMC)
# Objective : Generate features for use in GMJMCMC at the population transition step
# Created by: jonlachmann
# Created on: 2021-02-10

# Generate a multiplication feature
gen.multiplication <- function (features, marg.probs, trans.priors) {
  # Sample two features to be multiplied
  feats <- sample.int(n = length(features), size = 2, prob = marg.probs+0.00001, replace = TRUE)
  new_feature <- list("Prod",list(features[[feats[1]]]$eq,features[[feats[2]]]$eq))
  return(create.feature(new_feature, features[feats]$transforms, trans.priors))
}

# Generate a modification feature
gen.modification <- function (features, marg.probs, trans.probs, trans.priors) {
  feat <- sample.int(n = length(features), size = 1, prob = marg.probs+0.00001)
  trans <- sample.int(n = length(trans.probs), size = 1, prob = trans.probs)
  new_feature <- list(features[[feat]]$transforms[trans],features[[feat]]$eq)
  return(create.feature(new_feature, features[[feat]]$transforms, trans.priors))
}

# Generate a projection feature
gen.projection <- function (features, marg.probs, trans.probs, max.width, max.size, trans.priors) {
  if (!is.null(max.size)) {
    max.width <- min(max.width, max.size + 1)
  }
  feat.count <- sample.int(n = (min(max.width, (length(features)))-1), size = 1)
  feats <- sample.int(n = length(features), size = feat.count, prob = marg.probs+0.00001)
  trans <- sample.int(n = length(trans.probs), size = 1, prob = trans.probs)
  features_short <- features[feats]
  alphas <- rep(1, length(feats)+1)
  equations <- vector("list", length = length(feats))
  
  # Multiply each feature with the corresponding alpha
  for (i in c(1:length(feats))){
    current <- features_short[[i]]$eq
    equations[[i]] <- list("Prod",list(list(alphas[i+1]),current))
  }
  
  # Add the first alpha
  sum <- list("Sum",list(list(alphas[1]),sum_equations(equations)))
  # Transform the sum 
  new_feature <- list(transforms[trans],sum)
  return(create.feature(new_feature, features[1]$transforms, trans.priors, alphas))
}

# Generate new features from the initial covariates
gen.new <- function (features, F.0.size) {
  covariate <- sample.int(n = F.0.size, size = 1)
  return(features[[covariate]])
}

# Generate a new feature by deleting a part of an existing feature
gen.drop <- function (features, marg.probs, trans.priors) {
  feat <- sample.int(n = length(features), size = 1, prob = marg.probs+0.00001)
  # If no sum or prod in the expression, nothing to remove
  if (features[feat]$oc==0){
    return(NULL)
  }
  # Only drop parts of sums or products
  flat_elements <- unlist(features[[feat]]$eq)
  filtered_elements <- flat_elements[flat_elements %in% c(features[[1]]$transforms,"Sum", "Prod")]
  # Drop random part from equation
  remove_index <- sample.int(n=length(filtered_elements), 1)
  remove_out <- filtered_elements[remove_index]
  count <- sum(filtered_elements[1:remove_index]==remove_out)
  # Create new feature equation
  new_feature <- drop_switch_feature(features[[feat]]$eq,count,remove_out)
  return(create.feature(new_feature, features[[feat]]$transforms, trans.priors))
}

# Generate a new feature by switching a part of an existing feature by another feature
gen.switch <- function (features, marg.probs, trans.priors) {
  feats <- sample.int(n = length(features), size = 2, prob = marg.probs+0.00001)
  # If oc==0, this will just replace the feature with another exisiting feature
  if (features[[feats[1]]]$oc==0) {
    return(NULL)
  }
  # Switch out a random part of the feature's equation, but not the whole feature
  flat_elements <- unlist(features[[feats[1]]]$eq[-1])
  switch_index <- sample(c(1:length(flat_elements)), 1)
  switch_out <- flat_elements[switch_index]
  count <- sum(flat_elements[1:switch_index]==switch_out) # Make sure to switch out the correct feature 
  updated_part <- drop_switch_feature(features[[feats[1]]]$eq[-1],count,switch_out,features[[feats[2]]]$eq)
  new_feature <- c(features[[feats[1]]]$eq[1],updated_part)
  return(create.feature(new_feature, features[1]$transforms, trans.priors))
}

# Select a feature to generate and generate it
gen.feature <- function (features, marg.probs, data, loglik.alpha, probs, F.0.size, params, verbose = TRUE) {
  tries <- 0
  feat.ok <- F
  while (!feat.ok && tries < 50) {
    feat.type <- sample.int(n = 6, size = 1, prob = probs$gen)
    if (feat.type == 1) feat <- gen.multiplication(features, marg.probs, probs$trans_priors)
    if (feat.type == 2) feat <- gen.modification(features, marg.probs, probs$trans, probs$trans_priors, probs$trans_priors)
    if (feat.type == 3) feat <- gen.projection(features, marg.probs, probs$trans, params$L, params$max.proj.size, probs$trans_priors)
    if (feat.type == 4) feat <- gen.new(features, F.0.size)
    if (feat.type == 5) feat <- gen.removal(features, marg.probs, probs$trans_priors)
    if (feat.type == 6) feat <- gen.switch(features,marg.probs, probs$trans_priors)
    # Check that the feature is not too wide or deep
    if (!(feat$depth > params$D || feat$width > params$L)) {
      # Generate alphas using the strategy chosen
      if (params$alpha > 0) {
        feat <- gen.alphas(params$alpha, feat, data, loglik.alpha, verbose)
      }
      if (!is.null(feat)) {
        # Check for linear dependence of new the feature
        if (length(features) == F.0.size) feats <- list()
        else feats <- features[(F.0.size + 1):length(features)]
        if (params$check.col && !check.collinearity(feat, feats, F.0.size, data, params$col.check.mock.data))
          feat.ok <- T
        else if (!params$check.col)
          feat.ok <- T
      }
    }
    tries <- tries + 1
    params$eps <- min(params$eps + 0.01, 0.5)
    marg.probs <- pmin(pmax(marg.probs, params$eps), (1 - params$eps))
  }
  if (!feat.ok) return(NULL)
  else return(feat)
}

# Check if there is collinearity present in the current set of features
check.collinearity <- function (proposal, features, F.0.size, data, mock) {
  # Add the proposal to the feature list for evaluation
  features[[length(features) + 1]] <- proposal
  # Generate mock data to test with (avoiding too costly computations)
  if (mock)
    mock.data <- matrix(c(runif((F.0.size * 2), -100, 100), rep(1, F.0.size * 2),
                        runif((F.0.size * 2) * (F.0.size), -100, 100)), F.0.size * 2, F.0.size + 2)
  else
    mock.data <- check.data(data[seq_len(min(F.0.size * 2, dim(data)[1])), ], FALSE)
  # Use the mock data to precalc the features
  mock.data.precalc <- precalc.features(mock.data, features)
  # Fit a linear model with the mock data precalculated features
  linearmod <- lm(as.data.frame(mock.data.precalc[, -2]))
  # Check if all coefficients were possible to calculate
  if (sum(is.na(linearmod$coefficients)) == 0) return(FALSE)
  else return(TRUE)
}

# Generate features to represent the covariates, just takes the count needed
gen.covariates <- function (count, transforms) {
  covariates <- vector("list", length = count)
  for (i in c(1:count)){
    equation <- list(paste("x",i,sep=""))
    covariates[[i]] <- list(eq=equation, depth=0, width=1, oc=0, alphas=NULL, transforms=transforms)
    class(covariates[[i]]) <- "feature"
  }
  return(covariates)
}