# Title     : Features for use in GMJMCMC
# Objective : Define the features and get a useful way to turn them into a string
# Created by: jonlachmann
# Created on: 2021-02-10

# transform = 0 is a multiplication of the (two) features listed in f
# transform > 0 is a transform of the type denoted by the transform variable
# f is a list of features involved in the feature
# alpha is the coefficient before each feature listed in f,
# and also possibly one more for an intercept

# A feature list has the structure
# list(eq, depth, width, oc, alphas)

#' Create method for "feature" class
#'
#' @param transform A numeric denoting the transform type
#' @param features A list of features to include
#' @param trans.priors A vector of prior inclusion penalties for the different transformations.
#' @param alphas A numeric vector denoting the alphas to use
#' @noRd
create.feature <- function (equation, transforms, trans_priors, alphas=NULL) {
  # Given no alphas, assume no intercept and unit coefficients
  if (is.null(alphas)) alphas <- c(0, rep(1, length(features)))
  if (length(alphas) != (length(features) + 1)) stop("Invalid alpha/feature count")
  # Calculate the depth, operation count and width of the new feature
  depth <- calculate_depth(equation,transforms)
  oc <- calculate_oc(equation,transforms,trans_priors)
  width <- calculate_width(equation,transforms)
  # Generate the new feature list
  new_feature <- list(eq=equation, depth=depth, width=width, oc=oc, alphas=alphas, transforms=transforms)
  class(new_feature) <- "feature"
  return(new_feature)
}

#' Update alphas on a feature
#'
#' @param feature The feature to be updated
#' @param alphas The alphas that will be used
#' @param recurse If we are recursing, to note the number of alphas used
#' @noRd
update.alphas <- function (feature, alphas, recurse=FALSE) {
  feat <- feature[[length(feature)]]
  alpha <- 0
  # This is a more complex feature
  required_fields <- c("eq", "depth", "width", "oc", "alphas")
  if (is.list(feature) && all(required_fields %in% names(feature))) {
    # Adjust intercept if it is not multiplication
    if (feat[1,1] > 0 && nrow(feat) > 2) {
      alpha <- alpha + 1
      feat[1,3] <- alphas[alpha]
    }
    for (i in 2:nrow(feat)) {
      # Multiplication does not have alphas, and a zero intercept is no intercept
      if (feat[1,1] > 0 && feat[1,3] != 0) {
        alpha <- alpha + 1
        feat[i,3] <- alphas[alpha]
      }
      # If we have a nested feature, recurse into it
      if (is.list(feature[[feat[i,2]]])) {
        recur <- update.alphas(feature[[feat[i,2]]], alphas[alpha + seq_along(alphas)], TRUE)
        feature[[feat[i,2]]] <- recur$feature
        alpha <- alpha + recur$alpha
      }
    }
  }
  feature[[length(feature)]] <- feat
  if (recurse) return(list(alpha=alpha, feature=feature))
  else return(feature)
}

#' Print method for "feature" class
print.feature <- function(feature, labels=FALSE, round=FALSE){
  equation <- feature$eq
  transforms <- feature$transforms
  string <- convert_to_string(equation, transforms, labels, round)
  return(string)
}

# A function to get the depth of a feature
depth.feature <- function (feature) {
  required_fields <- c("eq", "depth", "width", "oc", "alphas", "transforms")
  if (is.list(feature) && all(required_fields %in% names(feature))) return(feature$depth)
  else stop("Invalid feature structure")
}

# A function to get the width of a feature
width.feature <- function (feature) {
  required_fields <- c("eq", "depth", "width", "oc", "alphas", "transforms")
  if (is.list(feature) && all(required_fields %in% names(feature))) return(feature$width)
  else stop("Invalid feature structure")
}

# A function to get the oc (operation count) of a feature
oc.feature <- function (feature) {
  required_fields <- c("eq", "depth", "width", "oc", "alphas", "transforms")
  if (is.list(feature) && all(required_fields %in% names(feature)))  return(feature$oc)
  else stop("Invalid feature structure")
}

# A function to get the complexity measures of a list of features
complex.features <- function (features) {
  featcount <- length(features)
  width <- rep(NA, featcount)
  oc <- rep(NA, featcount)
  depth <- rep(NA, featcount)
  for (i in 1:featcount) {
    width[i] <- width.feature(features[[i]])
    oc[i] <- oc.feature(features[[i]])
    depth[i] <- depth.feature(features[[i]])
  }
  return(list(width=width, oc=oc, depth=depth))
}
