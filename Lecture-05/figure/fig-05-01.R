library(pacman)
pacman::p_load(dplyr, magrittr, tidyr, withr, ggplot2)

m <- with_seed(2718,
               tibble(x    = runif(n   = 25,
                                   min = 0, 
                                   max = 1),
                      y    = runif(n   = 25,
                                   min = 0, 
                                   max = 1),
                      type = "Monte_Carlo"))

s <- with_seed(2718,
               seq(from       = 0, 
                   to         = 1, 
                   length.out = 5) %>%
                 expand_grid(., .) |> 
                 set_colnames(c("x", "y")) |>  
                 mutate(type = "Systematic"))

bind_rows(m, s) |> 
  mutate(across(.cols = type,
                .fns  = factor)) |> 
  ggplot() +
  aes(x = x, y = y) +
  geom_path(
    data = data.frame(
      x = 0 + 1 * cos(seq(0,.5 * pi,length.out = 100)),
      y = 0 + 1 * sin(seq(0,.5 * pi,length.out = 100))),
    aes(x = x, y = y))+
  geom_path(
    data = data.frame(
      x = c(0, 0, 1, 1, 0),
      y = c(1, 0, 0, 1, 1)),
    aes(x = x, y =y)) +
  geom_point(aes(colour = type),
             show.legend = FALSE) +
  facet_wrap(~type, ncol = 2) +
  theme_bw()