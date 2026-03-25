#' Insert Ns into gtsummary tables
#'
#' gtsummary, by default has suboptimal presentation of number of observations,
#' especially in terms of missing data. This function adds the code at
#' the cursor location of the active document.
#'
#' The first part should be placed after a gtsummary::tbl_summary call and the
#' add_by_n function definition should be moved to whereever R functions are
#' defined in your project
#'
#' This code only needs to be used for grouped tables. For ungrouped tables,
#' use \code{add_overall() |> add_n()}.
#'
#' Code modified from \href{https://stackoverflow.com/questions/68676585/gtsummary-split-add-n-between-groups}{a question on stackoverflow}
#'
#' @return adds gtsummary code to the active document
#' @export
add_gtsummary_n <- function(){
  message("For ungrouped tables, add_overall() |> add_n() is enough.")
  rstudioapi::insertText('
  # tbl_summary(by = ...) |>
  # add_overall() |>
  add_n() |>
  add_stat(
    fns = everything() ~ add_by_n
  ) |>
  modify_header(starts_with("add_n_stat") ~ "**N**") |>
  modify_table_body(
    ~ .x |>
      dplyr::relocate(add_n_stat_1, .before = stat_1) |>
      dplyr::relocate(add_n_stat_2, .before = stat_2)
      # if necessary, add additional groups here using the lines above as a model
  ) # |>
  # modify_spanning_header(
  #   c(all_stat_cols(F), starts_with("add_n_stat")) ~ "**Treatment Received**"
  # )


  ### MOVE THE FOLLOWING PART TO WHEREEVER YOU DEFINE FUNCTIONS E.G. 01_packages_functions.R ###
  add_by_n <- function(data, variable, by, ...) {
  data |>
    select(all_of(c(variable, by))) |>
    dplyr::arrange(pick(all_of(c(by, variable)))) |>
    dplyr::group_by(.data[[by]]) |>
    dplyr::summarise(val = sum(!is.na(.data[[variable]])), .groups = "drop") |>
    dplyr::mutate(
      by_col = paste0("add_n_stat_", dplyr::row_number()),
      val = style_number(val)
    ) |>
    # Pivot wider using the generated names
    dplyr::select(by_col, val) |>
    tidyr::pivot_wider(names_from = by_col, values_from = val)
  }
  ### END OF COPYING ###
  ')
  invisible()
}
