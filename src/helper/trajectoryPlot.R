sample_state <- pData(putamen_subset_cds)$State
lib_info_with_pseudo <- pData(putamen_subset_cds)

reduced_dim_coords <- reducedDimK(putamen_subset_cds)

ica_space_df <- Matrix::t(reduced_dim_coords) %>% as.data.frame() %>% 
  select_(prin_graph_dim_1 = 1, prin_graph_dim_2 = 2) %>% 
  mutate(sample_name = rownames(.), sample_state = rownames(.))
dp_mst <- minSpanningTree(putamen_subset_cds)
if (is.null(dp_mst)) {
  stop("You must first call orderCells() before using this function")
}
edge_df <- dp_mst %>% igraph::as_data_frame() %>% select_(source = "from", 
                                                          target = "to") %>%
  left_join(ica_space_df %>%
              select_(source = "sample_name",
                      source_prin_graph_dim_1 = "prin_graph_dim_1",
                      source_prin_graph_dim_2 = "prin_graph_dim_2"),
            by = "source") %>%
  left_join(ica_space_df %>% select_(target = "sample_name",
                                     target_prin_graph_dim_1 = "prin_graph_dim_1",
                                     target_prin_graph_dim_2 = "prin_graph_dim_2"),
            by = "target")
data_df <- t(monocle::reducedDimS(putamen_subset_cds)) %>% as.data.frame() %>% 
  select_(data_dim_1 = 1, data_dim_2 = 2) %>% rownames_to_column("sample_name") %>% 
  mutate(sample_state) %>% left_join(lib_info_with_pseudo %>% 
                                       rownames_to_column("sample_name"), by = "sample_name")


data_df %>% mutate(group = paste(TrajectoryState, stim),
                   ptime = round(Pseudotime)) %>% group_by(group, ptime) %>%
  count() -> streamgraph_dat

streamgraph(streamgraph_dat, key = "group", value = "n", date = "ptime",
            scale = "continuous",
            interactive=T) %>%
  sg_fill_tableau() %>%
  sg_legend(TRUE, "Group:")


d <- ggplot(data_df, aes(x = data_dim_1, y = data_dim_2))

d + geom_density_2d_filled(aes(color = CellType), contour_var = "count") +
  facet_grid(. ~ stim)

d + 
  stat_density_2d(geom = "raster",
                    aes(fill = after_stat(density)),
                    contour = FALSE) +
  geom_point(aes(color = CellType)) +
  facet_grid(. ~ stim) +
  scale_fill_viridis_c() +
  theme_cowplot()

data_df %>% mutate(x = as.numeric(round(data_dim_1)),
                   y = as.numeric(round(data_dim_1)) ) %>%
  group_by(stim, x, y) %>%
  count(name = "n") -> pointplot_data


ggplot(data_df, aes(x = round(data_dim_1), y = round(data_dim_2), color = CellType) ) +
  geom_area() +
  facet_wrap(~ stim) +
  theme_cowplot()
