compute_tsp_structure <- function(
  tree1_path,
  tree2_path,
  parameters,
  p = NULL,
  current_row = NULL,
  total_rows = NULL
) {
  cloud1 <- rlas::read.las(tree1_path, "xyz") |>
    coi::as_pt_cld() |>
    coi::voxelize(parameters[["vox_res"]]) |>
    coi::sor(6, 50)
  cloud2 <- rlas::read.las(tree2_path, "xyz") |>
    coi::as_pt_cld() |>
    coi::voxelize(parameters[["vox_res"]]) |>
    coi::sor(6, 50)

  size_tree1 <- nrow(cloud1)
  size_tree2 <- nrow(cloud2)

  z_1 <- as.numeric(cloud1[, 3])
  z_2 <- as.numeric(cloud2[, 3])
  # For some reason future_map posts warnings with max() min() but it must be
  # due to lazy loading since the results are still correct
  height_tree1 <- abs(max(z_1) - min(z_1))
  height_tree2 <- abs(max(z_2) - min(z_2))

  coi_val <- coi::coi(
    cloud_i = cloud1,
    cloud_j = cloud2,
    d_max = parameters[["coi_d_max"]],
    warnings = FALSE
  )

  cci_val <- coi::cci(
    cloud_i = cloud1,
    cloud_j = cloud2,
    strata_size = parameters[["cci_strata_size"]],
    hull_type = parameters[["cci_strata_hull_type"]],
    warnings = FALSE
  )

  ovlp_2d <- coi::co_2d(
    cloud_i = cloud1,
    cloud_j = cloud2,
    warnings = FALSE
  )

  sc1 <- coi::boxdim(
    cloud1,
    parameters[["boxdim_threshold"]],
    warnings = FALSE,
    plot = FALSE
  )

  sc2 <- coi::boxdim(
    cloud2,
    parameters[["boxdim_threshold"]],
    warnings = FALSE,
    plot = FALSE
  )
  if (!is.null(p)) p()

  list(
    coi = coi_val,
    cci = cci_val,
    ovlp_2d = ovlp_2d,
    sc_tree1 = round(sc1, 2),
    sc_tree2 = round(sc2, 2),
    size_tree1 = size_tree1,
    size_tree2 = size_tree2,
    height_tree1 = height_tree1,
    height_tree2 = height_tree2
  )
}

compute_tsp_coi_sensitivity <- function(
  tree1_path,
  tree2_path,
  dmax_values,
  vox_res,
  p = NULL
) {
  cloud1 <- rlas::read.las(tree1_path, "xyz") |>
    coi::as_pt_cld() |>
    coi::voxelize(vox_res)
  cloud2 <- rlas::read.las(tree2_path, "xyz") |>
    coi::as_pt_cld() |>
    coi::voxelize(vox_res)

  sensitivity <- tibble::tibble(d_max = dmax_values)
  sensitivity$coi <- vapply(
    sensitivity$d_max,
    function(d_max) {
      coi::coi(
        cloud_i = cloud1,
        cloud_j = cloud2,
        d_max = d_max,
        warnings = FALSE
      )
    },
    numeric(1)
  )

  if (!is.null(p)) p()

  sensitivity
}
