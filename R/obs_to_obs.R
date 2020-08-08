#' Filter Observation Sets
#' 
#' Create a new observation set from another with an arbitrary filter
#'
#' @param data a tidycohort
#' @param formula a formula of the form `new_column_name ~ target_column`
#' @param x the filter to be applied
#'
#' @return A tidycohort with a new column
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{filter_obs(X, obs_2 ~ observation, obs_type == 440922)}
filter_obs <- function(data, formula, x) {
    obs <- rlang::f_rhs(formula)
    name <- rlang::f_lhs(formula)
    
    data %>%
        dplyr::mutate(
            !!name := lapply(!!obs, function(j) {
                dplyr::filter(j, !!!rlang::enquo(x))
            })
        )
}