#' Cohort Tidy
#' 
#' Transform observational data into tidycohort format
#'
#' @param data a `data.frame` or `tibble` containing data of interest
#' @param ... column name assignments to be passed to `transmute`
#' @param name name for new nested observation set column--  a single character value
#'
#' @return A nested tidycohort tibble/ dataframe
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @examples 
#' \dontrun{
#' cohort_tidy(iris, id=Species, value_numeric=Sepal.Length, name="sepal_obs")
#' }
cohort_tidy <- function(data, ..., name="X1") {
    L <- rlang::enquos(...)
    
    stopifnot(
        all(names(L) %in% c("id", "start_time", "end_time", "obs_type", "value_code", "value_numeric", "value_text", "value_unit")), 
        "id" %in% names(L)
    )
    
    dplyr::transmute(data, !!!L) %>%
        dplyr::group_by(.data$id) %>%
        tidyr::nest() %>%
        `names<-`(c("id", name)) %>%
        dplyr::ungroup()
}