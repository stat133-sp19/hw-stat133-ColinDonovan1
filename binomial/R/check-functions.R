#1.1) Private Checker Functions
check_prob <- function(prob){
  if(prob <= 1 && prob >= 0 && length(prob) == 1){
    return(TRUE)
  }else{
    stop("prob value invalid, must be between 0 and 1 and of length 1")
  }
}

check_trials <- function(trials){
  if(trials >= 0 && trials %% 1 == 0){
    return(TRUE)
  }else{
    stop("invalid trials input, must be positive, non-zero integer")
  }
}

check_success <- function(success, trials){
  if(max(success) <= trials && is.vector(success) && max(success) >= 0 && success %% 1 == 0){
    return(TRUE)
  }else{
    stop("invalid success value, must be non-negative integer vector & less than # of trials")
  }
}