#1.2) Privare Auxilliary Functions

aux_mean <- function(trials, prob){
  aux_ave <- trials*prob
  aux_ave <- round(aux_ave, 7)
  return(aux_ave)
}

aux_variance <- function(trials, prob){
  aux_var <- trials*prob*(1 - prob)
  aux_var <- round(aux_var, 7)
  return(aux_var)
}

aux_mode <- function(trials, prob){
  if((trials*prob+prob) %% 1 == 0){
    aux_mod <- c((trials*prob + prob), (trials*prob + prob - 1))
    return(aux_mod)
  }else{
    aux_mod <- trials*prob
    return(aux_mod)
  }
}

aux_skewness <- function(trials, prob){
  aux_skew <- (1 - 2*prob)/(sqrt(prob*trials*(1 - prob)))
  aux_skew <- round(aux_skew, 7)
  return(aux_skew)
}

aux_kurtosis <- function(trials, prob){
  aux_kurt <- (1 - 6*prob*(1 - prob))/(prob*trials*(1 - prob))
  aux_kurt <- round(aux_kurt, 7)
  return(aux_kurt)
}