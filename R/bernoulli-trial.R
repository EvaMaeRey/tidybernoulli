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
bernoulli_trial <- function(prob = .25){

  data.frame(outcome = 0:1, prob = c(1-prob, prob))

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


                    # functions
                    init = function(trial = NULL){

                      self$trial <- trial
                      self$index <- 1

                      self$out <- cross_trials(self$trial, num_trials = self$index)

                      invisible(self)          #returns


                    },

                    update = function(increment = 1){ # a method

                      self$index <- self$index + increment

                      # displaying
                      self$out <- cross_trials(self$trial, num_trials = self$index)

                      invisible(self)          #returns

                    },

                    print = function() {  # print method; default is to print everything

                      print(self$out)

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
#' trial_init()
trial_init <- function(trial = NULL, prob = .25){

  if(is.null(trial)){trial = bernoulli_trial(prob = prob)}

  my_trials <- Trials$new()


  my_trials$init(trial = trial)

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
#' bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance()
#'
#' bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance(2)
#'
#' bernoulli_trial() |>
#' trial_init() |>
#'   trial_advance() |>
#'   trial_advance()
#'
trial_advance <- function(trials, increment = 1){

  my_trials <- trials

  my_trials$update(increment = increment)

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
#' bernoulli_trial() |>
#' trial_init() |>
#'   add_trials() |>
#'   add_trials()
add_trials <- function(trials, increment = 1){


  if(!R6::is.R6(trials)){my_trials <- trial_init(trial = trials)

  my_trials <- trial_advance(trials = my_trials,
                             increment = increment -1)

  }
  if(R6::is.R6(trials)){my_trials <- trial_advance(trials = trials,
                                               increment = increment)}

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








