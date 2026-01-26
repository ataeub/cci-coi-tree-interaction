calculate_dist <- function(data, traits = c("sla", "cn", "ldmc")) {
  data |>
    dplyr::group_by(tsp) |>
    dplyr::summarise(tsp_ltd = dist(
      dplyr::pick(dplyr::all_of(traits))
    )) |>
    dplyr::mutate(tsp_ltd = round(as.numeric(tsp_ltd), 3))
}

# create_tsp_community_table <- function(data, include_tsp = FALSE) {
#   tsp_community <- data |>
#     dplyr::select(tsp, n1:n10) |>
#     dplyr::distinct() |>
#     dplyr::mutate(dplyr::across(
#       n1:n10,
#       ~ stringr::str_replace(.x, "dead|missing", NA_character_)
#     )) |>
#     tidyr::pivot_longer(n1:n10,
#       names_prefix = "n",
#       names_to = "neighbour",
#       values_to = "species"
#     )
#   if (isTRUE(include_tsp)) {
#     tsps <- data |>
#       dplyr::select(tsp, species, tree) |>
#       dplyr::mutate(neighbour = "0", .after = tsp)
#     return(dplyr::bind_rows(tsp_community, tsps))
#   }
#   return(tsp_community)
# }

# make_mean_ln_traits <- function(trait_data, ln_data) {
#   mean_ln_traits <- list()
#   for (i in seq_along(ln_data$tsp)) {
#     current_row <- ln_data[i, ]
#     if (is.na(current_row$species)) {
#       current_traits <- trait_data |>
#         dplyr::slice(1) |>
#         dplyr::mutate_all(~NA) |>
#         dplyr::mutate(tsp = current_row$tsp)
#     } else {
#       current_traits <- trait_data |>
#         dplyr::filter(tsp != current_row$tsp &
#           species == current_row$species) |>
#         dplyr::summarise(dplyr::across(sla:p, ~
#           mean(.x, na.rm = TRUE))) |>
#         dplyr::mutate(
#           tsp = current_row$tsp, species = current_row$species,
#           .after = 0
#         )
#     }
#     current_traits <- current_traits |>
#       dplyr::mutate(neighbour = current_row$neighbour, .after = tsp)
#     mean_ln_traits[[i]] <- current_traits
#   }
#   dplyr::bind_rows(mean_ln_traits)
# }

# calculate_fdisp <- function(ln_trait_data) {
#   ln_fds <- list()
#   tsps <- unique(ln_trait_data$tsp)
#   for (i in seq_along(tsps)) {
#     current_tsp <- tsps[i]
#     current_trait_data <- ln_trait_data |>
#       dplyr::filter(tsp == current_tsp & !is.na(species)) |>
#       dplyr::select(species, tsp, sla:p) |>
#       dplyr::arrange(species)
#     trait_data <- current_trait_data |>
#       dplyr::select(-tsp) |>
#       dplyr::distinct() |>
#       as.data.frame()
#     if (nrow(trait_data) == 0) {
#       fd <- NA
#     } else if (nrow(trait_data) == 1) {
#       fd <- 0
#     } else {
#       rownames(trait_data) <- trait_data$species
#       trait_data$species <- NULL
#       trait_dist <- dist(trait_data)
#       abun <- current_trait_data |>
#         dplyr::select(tsp, species) |>
#         dplyr::count(species) |>
#         tidyr::pivot_wider(
#           names_from = species,
#           values_from = n,
#           values_fill = 0
#         ) |>
#         as.matrix()
#       fd <- FD::fdisp(trait_dist, abun)$FDis
#       cwm <- FD::functcomp(trait_data, abun) %>%
#         dplyr::rename_with(~ paste0(.x, "_cwm"))
#     }
#     current_fd <- dplyr::tibble(tsp = current_tsp, ln_fd = fd) |>
#       dplyr::bind_cols(cwm)
#     ln_fds[[i]] <- current_fd
#   }
#   dplyr::bind_rows(ln_fds)
# }

calculate_compositions <- function(tree_level_data) {
  ln_comp <- tree_level_data |>
    dplyr::select(tsp, tree, n1:n10) |>
    dplyr::group_by(tsp) |>
    dplyr::mutate(across(n1:n10, ~ replace(.x, .x %in% c("dead", "missing"), NA))) |>
    dplyr::summarize(ln_comp = {
      vals <- dplyr::c_across(n1:n10) |>
        unique() |>
        purrr::discard(is.na)
      paste0(vals, collapse = "_")
    }) |>
    dplyr::mutate(ln_comp = ifelse(ln_comp == "", "none", ln_comp))
  tsp_comp <- tree_level_data |>
    dplyr::select(tsp, tree, species) |>
    dplyr::group_by(tsp) |>
    dplyr::summarize(tsp_comp = paste(unique(species), collapse = "_"))
  out <- tree_level_data |>
    dplyr::left_join(tsp_comp, by = "tsp") |>
    dplyr::left_join(ln_comp, by = "tsp") |>
    dplyr::select(tsp, tsp_comp, ln_comp)
  out
}

calculate_tsp_traits <- function(tree_level_data) {
  tsp_dist <- calculate_dist(tree_level_data)
  # community_table <- create_tsp_community_table(tree_level_data)
  # traits_raw <- tree_level_data |>
  #   dplyr::select(tsp, species, sla:p)
  # mean_ln_trait_table <- make_mean_ln_traits(traits_raw, community_table)
  # fdisp <- calculate_fdisp(mean_ln_trait_table)
  compositions <- calculate_compositions(tree_level_data)
  traits <- tsp_dist |>
    # dplyr::left_join(fdisp, by = "tsp") |>
    dplyr::left_join(compositions, by = "tsp") |>
    dplyr::distinct()
  traits
}
