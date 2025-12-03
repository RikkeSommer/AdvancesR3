
#' Table 1 mean SD
#'
#' @param data: the dataset
#' @param col: the colum that should be grouped by
#'
#' @returns table 1
#' @export
#'
#' @examples
create_table_descriptive_stats <- function(data, col) {
  data |>
    dplyr::group_by({{ col }}) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) base::round(x, digits = 1))) |> # runder alle numeriske værdier til 1 decimal
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |> # sætter de to sammen i én værdi i tabellen og parenteserne er for at få sd i en parentes
    dplyr::select(Metabolite = {{ col }}, "Mean SD" = MeanSD) # "nyt navn = col i data", man vælger i select det, der skal være output i tabellen
}
