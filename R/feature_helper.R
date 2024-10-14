# Helper function for drop and switching feature
drop_switch_feature <- function(lst, count, target, replacement = NULL) {
  if (!(is.list(lst[[1]])) && (lst[[1]] == target) && is.null(replacement)){
    count <- count - 1
    if (count == 0) { # Count is to differentiate on equal operations, two sums etc. 
      if (target %in% c("Sum","Prod")){ # If Sum or Prod, remove one addend/factor
        which_keep <- sample.int(2,1)
        lst <- lst[[2]][[which_keep]]
      } else { # If a function, remove the modification and keep what is inside
        lst <- lst[[2]]
      }
      return(lst)
    }
  } 
  for (i in seq_along(lst)) {
    if (is.list(lst[[i]])) {
      # If the first element matches `switch`, replace the entire sublist
      if (!(is.list(lst[[i]][[1]])) && (lst[[i]][[1]] == target)) {
        count <- count - 1
        if (count == 0) {
          if (is.null(replacement)) {
            if (target %in% c("Sum","Prod")){ # If Sum or Prod, remove one addend/factor
              which_keep <- sample.int(2,1)
              lst[[i]] <- lst[[i]][[2]][[which_keep]]
            } else { # If a function, remove the modification and keep what is inside
              lst[[i]] <- lst[[i]][[2]]
            }
          } else {
            lst[[i]] <- replacement
          }
          return(lst)
        }
      } 
      lst[[i]] <- drop_switch_feature(lst[[i]], count, target, replacement)
    }
  }
  return(lst)  # Return the modified equation
}

# Helper function when making a projection, to sum over multiple features
sum_equations <- function(equations) {
  # Base case: if there's only one equation, return it
  if (length(equations) == 1) {
    return(equations[[1]])
  }
  # Recursive case: sum the first equation and the result of summing the rest
  return(list("Sum", list(equations[[1]], sum_equations(equations[-1]))))
}

# Helper function for print.feature
convert_to_string <- function(expression, transforms, labels = FALSE, round = FALSE, depth = 0, parenthesize = TRUE) {
  if (!(expression[[1]] %in% c(transforms,"Prod","Sum"))){
    depth <- depth+1
    if (is.numeric(round) && !(is.na(suppressWarnings(as.numeric(expression[[1]])))) ){
      return(round(as.numeric(expression[[1]]), digits = round(round,0)))
    }
    if (is.character(expression[[1]]) && startsWith(expression[[1]],"x")){
      index <- strtoi(sub("^x", "", expression[[1]]))
      if (labels[1]!=FALSE){
        return(labels[index])
      }
    }
    return(expression[[1]])
  }
  if (expression[[1]] %in% transforms){
    depth <- depth+1
    transform <- expression[[1]]
    variable <- convert_to_string(expression[[2]],transforms,labels,round,depth,parenthesize=FALSE)
    return(paste(transform,"(",variable,")",sep=""))
  }
  if (expression[[1]]=="Prod"){
    depth <- depth+1
    factor1 <- convert_to_string(expression[[2]][[1]],transforms,labels,round,depth,parenthesize=TRUE)
    factor2 <- convert_to_string(expression[[2]][[2]],transforms,labels,round,depth,parenthesize=TRUE)
    if (depth==1){
      return(paste(factor1,"*",factor2,sep=""))
    }
    else if (parenthesize){
      return(paste("(",factor1,"*",factor2,")",sep=""))
    } else { 
      return(paste(factor1,"*",factor2,sep=""))
    }
  }
  if (expression[[1]]=="Sum"){
    depth <- depth+1
    addend1 <- convert_to_string(expression[[2]][[1]],transforms,labels,round,depth,parenthesize=TRUE)
    addend2 <- convert_to_string(expression[[2]][[2]],transforms,labels,round,depth,parenthesize=TRUE)
    if (depth==1){
      return(paste(addend1,"+",addend2, sep=""))
    }
    else if (parenthesize){
      return(paste("(",addend1,"+",addend2,")", sep=""))
    } else {
      return(paste(addend1,"+",addend2, sep=""))
    }
  }
}

# Calculate oc
calculate_oc <- function(equation,transforms,trans_priors){
  flat_elements <- unlist(equation)
  filtered_transformed <- flat_elements[flat_elements %in% transforms]
  transform_sum <- sum(trans_priors[match(filtered_transformed,transforms)])
  filtered_rest <- flat_elements[flat_elements %in% c("Prod","Sum")]
  oc <- length(filtered_rest)+transform_sum
  return(oc)
}

# Calculate depth
calculate_depth <- function(equation,transforms){
  if (equation[[1]]=="Sum"){
    locdepth1 <- calculate_depth(equation[[2]][[1]],transforms)
    locdepth2 <- calculate_depth(equation[[2]][[2]],transforms)
    max_locdepth <- max(locdepth1,locdepth2)
    return(max_locdepth)
  } else if (equation[[1]] %in% transforms){
    locdepth <- 1+calculate_depth(equation[[2]],transforms)
    return(locdepth)
  } else if (equation[[1]]=="Prod"){
    locdepth1 <- calculate_depth(equation[[2]][[1]],transforms)
    locdepth2 <- calculate_depth(equation[[2]][[2]],transforms)
    locdepth <- locdepth1+locdepth2+1
    return(locdepth)
  } else {
    return(0)
  }
}

# Returns a list of the unique indexes of the x's in the equation, used for calculate width
unique_x <- function(equation,transforms,found_x = c()){
  if ((!(equation[[1]] %in% transforms)) && (is.character(equation[[1]])) && startsWith(equation[[1]],"x")) {
    index <- strtoi(sub("^x", "", equation[[1]]))
    if (!(index %in% found_x)){
      found_x <- c(found_x,index)
    }
  } else if (equation[[1]] %in% transforms){
    found_x <- unique_x(equation[[2]],transforms,found_x)
  } else if (equation[[1]] %in% c("Prod","Sum")){
    found_x <- unique_x(equation[[2]][[1]],transforms,found_x)
    found_x <- unique_x(equation[[2]][[2]],transforms,found_x)
  }
  return(found_x)
  
}

# Calculate width given equation
calculate_width <- function(equation,transforms){
  return(length(unique_x(equation,transforms)))
}