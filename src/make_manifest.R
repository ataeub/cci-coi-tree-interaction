make_manifest <- function(tree_table_path, single_tree_path) {
  paths <- fs::dir_ls(single_tree_path, glob = "*.laz") |>
    dplyr::tibble(pc_path = _) |>
    dplyr::mutate(pc_path = fs::path_rel(pc_path, here::here())) |>
    dplyr::mutate(tree = stringr::str_extract(
      pc_path,
      "(?:[A-Za-z]\\d{1,2}){1,2}_\\d{4}"
    ))

  tree_level_data <- readr::read_csv(tree_table_path)
  tree_level_data <- tree_level_data |>
    dplyr::left_join(paths, by = "tree")

  trees_without_clouds <- tree_level_data |>
    dplyr::filter(is.na(pc_path)) |>
    dplyr::pull(tsp) |>
    unique()
  warning(
    "The following trees have no point cloud:\n",
    paste(trees_without_clouds, collapse = "  ")
  )
  tree_level_data
}
