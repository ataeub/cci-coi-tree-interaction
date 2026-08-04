#' Build a tree pair edge or vertex manifest from beech LAZ files.
#'
#' Scans all `.laz` files in `input_dir`, computes the trunk centre of each
#' tree (points within 1 m of ground), and returns either a vertex table or an
#' edge table of within-dataset pairs whose trunk centres are no more than
#' `max_pair_dist` metres apart.
#'
#' Dataset and tree ID are parsed from the filename convention
#' `<dataset>_<tree>.laz` (e.g. `Schattin_S_A_001.laz`).
#'
#' @param input_dir   Path to the directory containing `.laz` files.
#' @param output_path (Currently unused) intended output path for the manifest.
#' @param max_pair_dist Maximum trunk-to-trunk distance (m) to include an edge.
#'   Default 15.
#' @param manifest_type One of `"edges"` (default) or `"vertices"`.
#' @param plot Unused placeholder for an optional plot.
#'
#' @return A tibble. For `"vertices"`: columns `name`, `x`, `y`, `dataset`,
#'   `path`. For `"edges"`: columns `tree_1`, `tree_2`, `dataset`, `x1`, `y1`,
#'   `x2`, `y2`, `path_1`, `path_2`, `trunk_dist`.
beech_auto_pair_manifest <- function(
  input_dir,
  max_pair_dist = 15,
  manifest_type = c("edges", "vertices"),
  plot = FALSE
) {
  manifest_type <- match.arg(manifest_type)

  trunk_center <- function(path) {
    cloud <- rlas::read.las(path, "xyz")
    trunk <- cloud[cloud$Z < (min(cloud$Z) + 1), ]
    trunk_center_x <- round(mean(trunk$X), 3)
    trunk_center_y <- round(mean(trunk$Y), 3)
    list(x = trunk_center_x, y = trunk_center_y)
  }

  file_paths <- fs::dir_ls(input_dir, glob = "*.laz")

  beech_centers <- dplyr::tibble(path = file_paths) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dataset = stringr::str_extract(path, "[^/]+(?=_\\d+\\.laz$)"),
      tree = stringr::str_extract(path, "\\d+(?=\\.laz$)"),
      center = list(trunk_center(path))
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest_wider(center, names_sep = "_")

  vertices <- beech_centers |>
    dplyr::select(name = tree, x = center_x, y = center_y, dataset, path)

  if (manifest_type == "vertices") {
    return(vertices)
  }

  edges <- dplyr::inner_join(
    dplyr::select(
      beech_centers,
      tree_1 = tree,
      dataset,
      x1 = center_x,
      y1 = center_y,
      path_1 = path
    ),
    dplyr::select(beech_centers,
      tree_2 = tree,
      dataset,
      x2 = center_x,
      y2 = center_y,
      path_2 = path
    ),
    by = "dataset", # Makes sure that pairs are only built within datasets
    relationship = "many-to-many"
  ) |>
    dplyr::filter(tree_1 < tree_2) |>
    dplyr::mutate(trunk_dist = round(sqrt((x2 - x1)^2 + (y2 - y1)^2), 3)) |>
    dplyr::filter(trunk_dist <= max_pair_dist)

  edges
}

#' Build a beech pair manifest from manually specified tree pairs.
#'
#' Takes a table of manually identified tree pairs, resolves each tree to a
#' `.laz` file in `beech_cloud_dir`, computes each trunk centre from points
#' within 1 m of ground level, and adds the trunk-to-trunk distance for every
#' pair.
#'
#' Tree IDs in `tree_1` and `tree_2` are zero-padded to three digits before
#' file paths are constructed using the convention
#' `<dataset>_<tree>.laz` (e.g. `Schattin_S_A_001.laz`).
#'
#' @param pairs_raw A data frame or tibble containing at least `dataset`,
#'   `tree_1`, and `tree_2` columns.
#' @param beech_cloud_dir Path to the directory containing processed beech LAZ
#'   files. Defaults to `"data/processed/beech_data"`.
#'
#' @return A tibble with the input pair data plus `path_1`, `path_2`,
#'   `center_1_x`, `center_1_y`, `center_2_x`, `center_2_y`, and
#'   `trunk_dist`.
beech_manual_manifest <- function(
  pairs_raw,
  beech_cloud_dir = "data/processed/beech_data"
) {
  trunk_center <- function(path) {
    cloud <- rlas::read.las(path, "xyz")
    trunk <- cloud[cloud$Z < (min(cloud$Z) + 1), ]
    trunk_center_x <- round(mean(trunk$X), 3)
    trunk_center_y <- round(mean(trunk$Y), 3)
    list(x = trunk_center_x, y = trunk_center_y)
  }

  pairs <- pairs_raw |>
    dplyr::mutate(
      dplyr::across(c(tree_1, tree_2), \(x) stringr::str_pad(x, 3, pad = "0")),
      path_1 = fs::path(
        beech_cloud_dir, glue::glue("{dataset}_{tree_1}.laz")
      ),
      path_2 = fs::path(
        beech_cloud_dir, glue::glue("{dataset}_{tree_2}.laz")
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      center_1 = list(trunk_center(path_1)),
      center_2 = list(trunk_center(path_2)),
      trunk_dist = round(
        sqrt((center_2$x - center_1$x)^2 + (center_2$y - center_1$y)^2),
        3
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest_wider(center_1, names_sep = "_") |>
    tidyr::unnest_wider(center_2, names_sep = "_")
}

#' Convert ASCII point cloud files to LAZ format for beech files
#'
#' Reads XYZ or ASCII point cloud files from \code{input_dir}, applies
#' site-specific coordinate offsets, standardises file names, and writes
#' the result as LAZ files to \code{output_dir}.
#'
#' File names are normalised as follows:
#' \itemize{
#'   \item \code{Big_Tile_[1,2]} → \code{Serrahn}
#'   \item \code{Schattin_Sued_A} → \code{Schattin_S_A}
#'   \item \code{Schattin_Sued_B} → \code{Schattin_S_B}
#'   \item Numeric suffix is zero-padded to three digits.
#' }
#'
#' @param input_dir          Path to the directory containing raw point cloud files.
#' @param output_dir         Path to the directory where LAZ files will be written.
#' @param glob               Glob pattern used to select input files.
#'                           Defaults to \code{"*.xyz|*.ascii"}.
#' @param serrahn_offset     \code{c(x, y)} offset subtracted from \code{Big_Tile} coordinates.
#' @param schattin_a_offset  \code{c(x, y)} offset subtracted from \code{Schattin_Sued_A} coordinates.
#' @param schattin_b_offset  \code{c(x, y)} offset subtracted from \code{Schattin_Sued_B} coordinates.
#'
#' @return Called for its side effect of writing LAZ files; returns \code{NULL} invisibly.
.beech_ascii_to_laz <- function(
  input_dir,
  output_dir,
  glob = "*.xyz|*.ascii",
  serrahn_offset = c(380000, 5911000),
  schattin_a_offset = c(618000, 5960700),
  schattin_b_offset = c(0, 0)
) {
  raw_file_paths <- fs::dir_ls(input_dir, glob = glob)

  for (path in raw_file_paths) {
    # # Can be used to skip the first X files
    # idx <- match(path, raw_file_paths)
    # if (idx < 50) {next()}
    file_name <- fs::path_file(path)
    file_ext <- fs::path_ext(file_name)
    delimiter <- ifelse(file_ext == "xyz", " ", ",")
    output_file_name <- file_name |>
      fs::path_ext_set("laz")

    output_file_name <- output_file_name |>
      stringr::str_replace("Big_Tile_[1,2]", "Serrahn") |>
      stringr::str_replace("Schattin_Sued_A", "Schattin_S_A") |>
      stringr::str_replace("Schattin_Sued_B", "Schattin_S_B")

    num <- stringr::str_extract(output_file_name, "\\d+(?=\\.laz$)")
    output_file_name <- stringr::str_replace(
      output_file_name, "\\d+(?=\\.laz$)", sprintf("%03d", as.integer(num))
    )

    output_file_path <- fs::path(output_dir, output_file_name)

    message(file_name, " --> ", output_file_name)

    cloud <- readr::read_delim(
      path,
      delim = delimiter,
      show_col_types = FALSE,
      col_names = c("X", "Y", "Z")
    ) |>
      # # Can be used to inspect the mean coordinates of a datasets xy coords
      # if(!grepl("Schattin_Sued_B", file_name)) next()
      # print(mean(cloud$X))
      # print(mean(cloud$Y))
      # next()

      dplyr::mutate(
        X = {
          if (grepl("Big_Tile", file_name)) {
            X - serrahn_offset[1]
          } else if (grepl("Schattin_Sued_A", file_name)) {
            X - schattin_a_offset[1]
          } else if (grepl("Schattin_Sued_B", file_name)) {
            X - schattin_b_offset[1]
          } else {
            X
          }
        },
        Y = {
          if (grepl("Big_Tile", file_name)) {
            Y - serrahn_offset[2]
          } else if (grepl("Schattin_Sued_A", file_name)) {
            Y - schattin_a_offset[2]
          } else if (grepl("Schattin_Sued_B", file_name)) {
            Y - schattin_b_offset[2]
          } else {
            Y
          }
        }
      )
    header <- rlas::header_create(cloud)

    rlas::write.las(output_file_path, header, cloud)
  }
}