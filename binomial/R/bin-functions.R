#1.3) Function bin_choose()

#' @title Binomial Coefficient
#' @description represents the number of ways one can choose k objects from n number of objects
#' @param n total number of objects, any integer greater than zero
#' @param k number of objects to be selected, any integer or vector less than n
#' @return binomial coefficient, or the number of ways to choose k objects from n # of objects
#' @export
#' @examples bin_choose(n = 10, k = 3)
#' @examples bin_choose(5, 2)
#' @examples bin_choose(10, 0:5)

bin_choose <- function(n, k){
  if(k > n){
    return("n must be greater than k")
    stop()
  }else{
    choice <- factorial(n)/(factorial(k)*factorial(n - k))
    return(choice)
  }
}

#1.4) Function bin_probability()

#' @title Binomial Probability
#' @description gives the probability of something occurring over a specified number of trials
#' @param success integer, less than or equal to the # of trials
#' @param trials which must be an integer or vector of integers, greater than zero
#' @param prob a number between 0 and 1
#' @return returns the probability of a # of successes occurring for a given number of trials
#' @export
#' @examples bin_probability(success = 10, trials = 15, prob = 0.5)
#' @examples bin_probability(5, 10, )

bin_probability <- function(success, trials, prob){
  if(check_success(success, trials) != TRUE || check_trials(trials) != TRUE || check_prob(prob) != TRUE){
    return("invalid input value")
    stop()
  }else{
    bin_choice <- bin_choose(n = trials, k = success)
    bin_prob <- (prob^(success))*((1 - prob)^(trials - success))
    return(bin_prob*bin_choice)
  }
}

#1.5) Function bin_distribution()

#' @title Binomial Distribution
#' @description probability distribution for a given number of trials, output can also be directly plotted using plot()
#' @param trials must be an integer, greater than zero
#' @param prob a number between 0 and 1
#' @return a dataframe with the # of successes, and the associated probability distribution
#' @export
#' @examples bin_distribution(trials = 10, prob = 0.5)
#' @examples bin_distribution(8, 0.4)

bin_distribution <- function(trials, prob){
  check_prob(prob)
  check_trials(trials)
  successes <- c(0:trials)
  prob <- bin_probability(success = 0:trials, trials = trials, prob = prob)
  dat <- data.frame(successes, prob)
  class(dat) <- c("bindis", "data.frame")
  return(dat)
}

#' @export
plot.bindis <- function(x){
  Plot <- barplot(x$prob, xlab = "successes", ylab = "probability", names.arg = x$success)
  return(Plot)
}

#1.6) Function bin_cumulative()

#' @title Binomial Cumulative
#' @description will give a data frame of both the probabilities and the cumulative sum associated with a given number of successes.Can then be plotted directly using plot() function.
#' @param trials must be an integer, greater than zero
#' @param prob a number between 0 and 1
#' @return data frame with # of successes, probability, and cumulative sum
#' @export
#' @examples bin_cumulative(trials = 5, prob = 0.5)
#' @examples bin_cumulative(10, 0.2)

bin_cumulative <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  success <- c(0:trials)
  probability <- bin_probability(success = 0:trials, trials = trials, prob = prob)
  cumulative <- cumsum(probability)
  dat2 <- data.frame(success, probability, cumulative)
  class(dat2) <- c("bincum", "data.frame")
  return(dat2)
}

#' @export

plot.bincum <- function(x){
  Plot <- plot(x$success, x$cumulative, xlab = "successes", ylab = "probability", type = "o")
  return(Plot)
}

#1.7) Function bin_variable()

#' @title Binomial Variable
#' @description function which allows you to format your trials and prob, as well as summarizing using the print() or summary(), which will give you more information on your binomial distribution
#' @param trials which must be an integer or vector of integers, greater than zero
#' @param prob which is a number between 0 and 1
#' @return outputs your number of trials, and the probability of success in a formatted way
#' @export
#' @examples bin_variable(trials = 5, prob = 0.5)
#' @examples bin_variable(10, 0.3)
#' @examples bin_variable(1:10, 0.4)

bin_variable <- function(trials, prob){
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE){
    listed <- list(trials, prob)
    names(listed) <- c("trials", "prob")
    class(listed) <- "binvar"
    return(listed)
  }else{
    stop("not valid trials or prob input")
  }
}

#' @export

print.binvar <- function(x){
  if(class(x) == 'binvar'){
    cat('"Binomial Variable"')
    cat("\n\n", append = TRUE)
    cat("Parameters", append = TRUE)
    cat("\n-number of trials:", x$trials, append = TRUE)
    cat("\n-prob of success:", x$prob, append = TRUE)
  }else{
    stop("incorrect object type")
  }
}

#' @export

summary.binvar <- function(x){
  if(class(x) == 'binvar'){
    returned <- list(trials <- x$trials,
                     prob <- x$prob,
                     aux_mean(trials, prob),
                     aux_variance(trials, prob),
                     aux_mode(trials, prob),
                     aux_skewness(trials, prob),
                     aux_kurtosis(trials, prob))
    names(returned) <- c("trials", "prob", "mean", "variance", "mode", "skewness", "kurtosis")
    class(returned) <- 'summary.binvar'
    return(returned)
  }else{
    stop("incorrect object type")
  }
}

#' @export

print.summary.binvar <- function(x){
  cat('"Binomial Variable"')
  cat("\n\n", append = TRUE)
  cat("Parameters", append = TRUE)
  cat("\n-number of trials:", append = TRUE)
  cat("\n-prob of success:", append = TRUE)
  cat("\n\nMeasures\n", append = TRUE)
  cat("-mean:", x$mean, "\n-variance:", x$variance, "\n-mode:", x$mode,
      "\n-skewness:", x$skewness, "\n-kurtosis:", x$kurtosis)
}

#1.8) Functions of Measure

#' @export
bin_mean <- function(trials, prob){
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE){
    return(aux_mean(trials, prob))
  }else{
    stop("incorrect trial or prob input")
  }
}

#' @export
bin_mode <- function(trials, prob){
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE){
    return(aux_mode(trials, prob))
  }else{
    stop("incorrect trial or prob input")
  }
}

#' @export
bin_skewness <- function(trials, prob){
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE){
    return(aux_skewness(trials, prob))
  }else{
    stop("incorrect trial or prob input")
  }
}

#' @export
bin_kurtosis <- function(trials, prob){
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE){
    return(aux_kurtosis(trials, prob))
  }else{
    stop("incorrect trial or prob input")
  }
}

#' @export
bin_variance <- function(trials, prob){
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE){
    return(aux_variance(trials, prob))
  }else{
    stop("incorrect trial or prob input")
  }
}