library(pacman)
p_load(tidyr)
p_load(tibble)
p_load(ggplot2)
anscombe |> 
  rowid_to_column("id") |> 
  pivot_longer(-id,
               names_to        = c("axis", "group"),
               names_sep       = 1L,
               names_transform = list(group = as.factor)) |> 
  pivot_wider(c(id, group), 
              names_from = axis) |> 
  dplyr::select(-id) |> 
  ggplot() +
  aes(x = x, y = y) +
  geom_point() +
  geom_smooth(method    = "lm",
              #              se        = FALSE,
              fullrange = TRUE) +
  facet_wrap(~ group,
             nrow  = 2,
             scale = "fixed")