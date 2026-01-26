trees_to_tsps <- function(tree_table, verbose = TRUE) {
    tsp_table <- tree_table |>
        dplyr::select(c(
            plot,
            tsp,
            tree,
            tsp_div,
            tsp_habits,
            ln_sr,
            ln_n,
            matches("^pc_path")
        )) |>
        dplyr::group_by(plot, tsp) |>
        dplyr::mutate(tree_num = paste0("tree", dplyr::row_number())) |>
        tidyr::pivot_wider(
            id_cols = c(plot, 
            tsp, 
            tsp_div,
            tsp_habits,
            ln_sr,
            ln_n),
            names_from = tree_num,
            values_from = c(tree, pc_path, matches("^pc_path")),
            names_glue = "{tree_num}_{.value}"
        ) |>
        dplyr::mutate(valid_row = ifelse(is.na(tree1_pc_path) |
            is.na(tree2_pc_path),
        FALSE,
        TRUE
        ))
    invalid_rows <- tsp_table |>
        dplyr::filter(valid_row == FALSE) |>
        nrow()
    if (invalid_rows > 0 & isTRUE(verbose)) {
        warning(invalid_rows, " TSPs do not have two point clouds!")
    }
    tsp_table |> dplyr::select(-valid_row)
}
