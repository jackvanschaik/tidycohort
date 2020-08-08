#' Observation Sets to Vectors
#' 
#' Summarize an observation set (nested data frames) to a single value for each patient
#' 
#' @param data a tidycohort data frame
#' @param formula a formula of the form `new_column_name ~ target_column`
#' 
#' @return A tidycohort with a new column determined by the LHS of `forumla`
#' 
#' @name obs_to_scalar
#' 
#' @md
NULL

#' @rdname obs_to_scalar
#'
#' @param summary the summary statement to be used 
#' 
#' @export
#'
#' @examples \dontrun{summarise_obs(X, new_col ~ old_col, sum(obs_type == "xyz"))}
summarise_obs <- function(data, formula, summary) {
    obs <- rlang::f_rhs(formula)
    name <- rlang::f_lhs(formula)
    summary <- rlang::enquo(summary)
    
    data %>%
        dplyr::mutate(!!name := purrr::map(!!obs, function(x) dplyr::summarise(x, y=!!summary))) %>%
        dplyr::mutate(!!name := unlist(!!name))
}

#' @rdname obs_to_scalar
#' 
#' @details `any_obs_of_type()` checks for observations of type `obs_type` in an observation set
#' 
#' @param type the observation type, a typical `obs_type` in the observation set
#'
#' @export
#' 
#' @importFrom rlang .data
any_obs_of_type <- function(data, formula, type) {
    type <- rlang::enquo(type)
    summarise_obs(data, formula, as.numeric(sum(.data$obs_type == !!type) >= 1))
}

#' @rdname obs_to_scalar
#' 
#' @details `any_obs()` checks for any observations at all in an observation set
#'
#' @export
#'
#' @examples \dontrun{any_obs(X, any_diagnoses_obs ~diagnoses)}
any_obs <- function(data, formula) {
    obs <- rlang::f_rhs(formula)
    name <- rlang::f_lhs(formula)
    
    data %>%
        dplyr::mutate(!!name := purrr::map(!!obs, function(x) {
            if(is.data.frame(x)) {
                if (nrow(x) > 0) {
                    return(TRUE)
                }
            }
            return(FALSE)
        })) %>%
        dplyr::mutate(!!name := unlist(!!name))
}

#' @rdname obs_to_scalar
#'
#' @details `index_time()` extracts the earliest `start_time` in an observation set
#'
#' @export
#'
#' @examples \dontrun{index_time(Y, diabetes_index_time ~ diabetes)}
index_time <- function(data, formula) {
    obs <- rlang::f_rhs(formula)
    name <- rlang::f_lhs(formula)
    
    data %>%
        dplyr::mutate(!!name := purrr::map(!!obs, function(x) {
            if(nrow(x) > 0 ) {
                return(min(x$start_time))
            }
            else {
                return(as.Date(NA))
            }
        }))
}

