
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

######################################

#' Making a histogram
#'
#' @param data the dataset (here lipidomics)
#' @param col the varibale to group by
#' @param value the value that should be as a histogram
#'
#' @returns A histogram
#' @export
#'
#' @examples
create_plot_distributions <- function(data, col) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)
    ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars({{col}}), scales = "free") + # free: for each it can define its own axis values. facetwrap is the one we want to "group:by"
    ggplot2::theme_minimal()

}

################################################


#' Title Cleaning data: values to mean values of each metabolite
#'
#' @param data lipidomics data frame
#'
#' @returns a dataframe with mean values
#' @export
#'
#' @examples
clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>  #grupper på alt andet end value
    dplyr::summarise(value = mean(value), .groups = "keep") |>  #summér value som mean af værdierne og behold kolonnerne i group_by
    dplyr::ungroup()
}

##################################

#' Title: Mean centering values for comparison
#'
#' @param data Lipidomics data
#'
#' @returns A data.frame with mean.centered values
preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}

#########################
#' Title function to create a model
#'
#' @param data lipidomics data
#' @param model OR
#'
#' @returns

fit_model = function(data, model) {
  stats::glm(
    formula = model,
    data = data,
    family = binomial
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = dplyr::everything()
    )
}

#########################

#' Title Cholesterol model
#'
#' @param data lipidomics
#'
#' @returns results for cholesterols

create_model_results = function(data) {
  data |>
    dplyr::filter(metabolite == "Cholesterol") |>
    preprocess() |>
    fit_model(class ~ value)
}
