#' Title
#'
#' @param prob probability of success, default is .25
#'
#' @return probability and outcome as 0 and 1
#' @export
#'
#' @examples
#' bernoulli_trial(.5)
#' bernoulli_trial(.6)
bernoulli_trial <- function(outcome_type = "num", outcome_set = c(0, 1), prob = .25){

  if(outcome_type == "char"){
    data <- data.frame(outcome = outcome_set, prob = c(1-prob, prob))
  }

  if(outcome_type == "num"){
    data <- data.frame(outcome = 0:1, prob = c(1-prob, prob))
  }

  if(outcome_type == "logical"){
    data <- data.frame(outcome = c(FALSE, TRUE), prob = c(1-prob, prob))
  }

  data

}










#' Title
#'
#' @param prob probability of success, default is .25
#'
#' @return probability and outcome as 0 and 1
#' @export
#'
#' @examples
#' weighted_coin()
#' weighted_coin(prob = .55)
#' weighted_coin(outcome_type = "num")
#' weighted_coin(outcome_type = "logical")
#' weighted_coin(outcome_set = c("fish", "no fish"))
weighted_coin <- function(prob = .75, outcome_type = "char", outcome_set = c("tails","heads")){

  bernoulli_trial(outcome_type = outcome_type, outcome_set = outcome_set, prob = prob)

}


#' Title
#'
#' @param prob probability of success, default is .25
#'
#' @return probability and outcome as 0 and 1
#' @export
#'
#' @examples
#' fair_coin()
#' fair_coin(outcome_type = "num")
#' fair_coin(outcome_type = "logical")
#' fair_coin(outcome_set = c("fish", "no fish"))
fair_coin <- function(outcome_type = "char", outcome_set = c("tails","heads")){

  # same as weighted but only prob point five allowed
  bernoulli_trial(outcome_type = outcome_type, outcome_set = outcome_set, prob = .5)

}


#' Title
#'
#' @param back_story
#'
#' @return
#' @export
#'
#' @examples
#'
#' prize_wheel()
#' prize_wheel(back_story = FALSE)
#'
#' prize_wheel() |>
#'  trial_init() |>
#'  trial_advance()
#'
#' prize_wheel(back_story = FALSE) |>
#'  trial_init() |>
#'  trial_advance()
#'
prize_wheel <- function(back_story = TRUE){

  if(back_story){

  data <- data.frame(outcome = c("$0", "$1", "$3"), prob = c("9 of 12 slices", "2 of 12 slices", "1 of 12 slice"))

  }else{

  data <- data.frame(outcome = c(0, 1, 3), prob = c(9/12, 2/12, 1/12))

  }

  data
}

#' Title
#'
#' @param trial
#' @param num_trials
#'
#' @return
#' @export
#'
#' @examples
#'
#' bernoulli_trial() |>
#'   cross_trials(num_trials = 3)
cross_trials <- function(trial, num_trials = 2){

  df <- trial
  names(df) <- paste0("t", 1,"_", names(df))

  if(num_trials > 1){
    for (i in 2:num_trials){

      temp <- trial
      names(temp) <- paste0("t", i,"_", names(trial))

      df <- tidyr::crossing(df, temp)

    }
  }

  df

}





# my_trials <- Trials$new()
#
# my_trials
#
# my_trials$init(trial = bernoulli_trial())
# my_trials$out
# my_trials$update()
# my_trials$out

Trials <- R6::R6Class("Trials",
                  public = list(

                    # objects
                    trial = NULL,
                    index = NULL,
                    out = NULL,
                    out_ts = NULL,
                    as_ts = FALSE,


                    # functions
                    init = function(trial = NULL){

                      self$trial <- trial
                      self$index <- 1

                      self$out <- cross_trials(self$trial,
                                               num_trials = self$index) |>
                        dplyr::mutate(history = dplyr::row_number()) |>
                        dplyr::select(.data$history, dplyr::everything())

                      invisible(self)          #returns


                    },

                    update = function(increment = 1){ # a method

                      self$index <- self$index + increment


                      # displaying
                      self$out <- cross_trials(self$trial, num_trials = self$index)  |>
                        dplyr::mutate(history = dplyr::row_number()) |>
                        dplyr::select(.data$history, dplyr::everything())

                      invisible(self)          #returns

                    },

                    to_time_series = function(as_ts = FALSE){

                      self$as_ts <- as_ts

                    },

                    print = function() {  # print method; default is to print everything

                      if(self$as_ts){

                            print(

                            self$out |>
                              tidyr::pivot_longer (
                                cols = -history, names_sep = "_",
                                names_to = c("trial","name")
                                   ) |>
                              tidyr::pivot_wider(names_from = .data$name,
                                                 values_from = .data$value)

                            )

                        }else{print(self$out)}

                    }
                  )
)







#' Title
#'
#' @param trial
#'
#' @return
#' @export
#'
#' @examples
#' bernoulli_trial() |>
#' trial_init()
#'
#' bernoulli_trial() |>
#' trial_init(as_ts = TRUE)
#'
#' trial_init() -> hi
trial_init <- function(trial = NULL, prob = .25, as_ts = FALSE){

  if(is.null(trial)){trial = bernoulli_trial(prob = prob)}

  my_trials <- Trials$new()


  my_trials$init(trial = trial)

  my_trials$to_time_series(as_ts = as_ts)

  my_trials

}


#' Title
#'
#' @param trials
#' @param increment
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance()
#'
#' bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance(2)
#'
#'bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance(2, as_ts = TRUE)

#'
#' bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance() |>
#'   trial_advance()
#'
trial_advance <- function(trials, increment = 1, as_ts = FALSE){

  my_trials <- trials

  my_trials$update(increment = increment)
  my_trials$to_time_series(as_ts = as_ts)

  my_trials

}


#' Title
#'
#' @param trials
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' bernoulli_trial() |>
#'   add_trials() |>
#'   to_tsibble()
#'
#' bernoulli_trial(prob = .5) |>
#'   add_trials() |>
#'   add_trials() |>
#'   to_tsibble()  |>
#'   group_by(history)  |>
#'   summarize(hist_prob = prod(prob),
#'             count_successes = sum(outcome),
#'             paths = paste(outcome, collapse = ",")) |>
#'   arrange(count_successes) |>
#'   group_by(count_successes) |>
#'   summarize(count_prob = sum(hist_prob))
#'
to_tsibble <- function(trials){

  my_trials <- trials

  my_trials$update(increment = 0)

  my_trials$to_time_series(as_ts = TRUE)

  my_trials$out |>
    tidyr::pivot_longer(
      cols = -.data$history, names_sep = "_",
      names_to = c("trial","name")
    ) |>
    tidyr::pivot_wider(names_from = .data$name,
                       values_from = .data$value)

}


#' Title
#'
#' @param trials
#' @param increment
#'
#' @return
#' @export
#'
#' @examples
#' bernoulli_trial() |>
#' trial_init() |>
#'   add_trials() |>
#'   add_trials()
#'
#'bernoulli_trial() |>
#'   add_trials()
#'
#' bernoulli_trial() |>
#'   add_trials() |>
#'   add_trials(as_ts = TRUE)
#'
#'  fair_coin() |>
#'   add_trials(2)
add_trials <- function(trials, increment = 1, as_ts = FALSE){

  if(!R6::is.R6(trials)){

    my_trials <- trial_init(trial = trials,
                            as_ts = as_ts)

    my_trials <- trial_advance(trials = my_trials,
                              increment = increment,
                              as_ts = as_ts)

  }

  if(R6::is.R6(trials)){

    my_trials <- trial_advance(trials = trials,
                               increment = increment,
                               as_ts = as_ts)

    }

  my_trials

}






#' Title
#'
#' @param data
#' @param var_key
#'
#' @return
#' @export
#'
#' @examples
sum_across <- function(data, var_key = "outcome"){

  dplyr::mutate(.data = data,
                global_outcome =
                  rowSums(dplyr::across(dplyr::contains(var_key)))) |>
    dplyr::select(global_outcome, dplyr::everything())

}

#' Title
#'
#' @param data
#' @param var_key
#'
#' @return
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' bernoulli_trial(prob = .5) %>%
#' add_trials(3) %>%
#'   .$out %>%
#'   seq_across() %>%
#'   prod_across() %>%
#'   group_by(global_outcome) %>%
#'   summarize(probs = sum(global_probs))
#'
#' library(magrittr)
#' library(dplyr)
#' bernoulli_trial(prob = .25) %>%
#' add_trials(3) %>%
#'   .$out %>%
#'   seq_across() %>%
#'   prod_across()
seq_across <- function(data, var_key = "outcome"){

  col_list <- names(data)[names(data) %>% stringr::str_detect(var_key)]

  paste_collapse <- function(x){paste(x, collapse = ", ")}

  data$global_outcome  <- apply(data[,col_list], MARGIN = 1, FUN = paste_collapse)

  dplyr::select(data, global_outcome, everything())

}




#' Title
#'
#' @param data
#' @param var_key
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' bernoulli_trial(prob = .5) %>%
#'   add_trials() %>%
#'   add_trials() %>%
#'   add_trials(5) %>%
#'   .$out %>%
#'   sum_across() %>%
#'   prod_across() %>%
#'   group_by(global_outcome) %>%
#'   summarize(probs = sum(global_probs))
#'
#'
#' bernoulli_trial(prob = .5) %>%
#'   add_trials() %>%
#'   add_trials() %>%
#'   add_trials(5) %>%
#'   .$out %>%
#'   sum_across() %>%
#'   prod_across()
prod_across <- function(data, var_key = "prob"){

  col_list <- names(data)[names(data) %>% stringr::str_detect(var_key)]

  data$global_probs  <- apply(data[,col_list], MARGIN = 1, FUN = prod)

  dplyr::select(data, global_probs, everything())

}








