library(pacman)
p_load(withr, MASS, ggplot2)

dat_pe <- with_seed(2718,
                    mvrnorm(n         = 100,
                            mu        = c(0, 0), 
                            Sigma     = matrix(c(1, 0.7, 0.7, 1),
                                               ncol = 2),
                            empirical = FALSE)) |> 
  as.data.frame() |> 
  dplyr::rename(x = 1, y = 2)

dat_sp <- dat_pe |> 
  mutate(across(everything(),
                row_number))


bind_rows(dat_pe |>  mutate(type = "Pearson"),
          dat_sp |>  mutate(type = "Spearman")) |>  
  ggplot() +
  aes(x = x, y = y) +
  geom_point() +
  facet_wrap(~ type, ncol = 2, scales = "free")