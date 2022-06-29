library(pacman)
p_load(withr, purrr, dplyr, MASS, ggplot2)

r <- seq(from       = -1, 
         to         = 1, 
         length.out = 9)
with_seed(27182,
          r %>%
            map(~mvrnorm(n  = 100,
                         mu = c(0, 0), 
                         Sigma = matrix(c(1, ., ., 1), 
                                        ncol = 2),
                         empirical = TRUE)) |> 
            map(as.data.frame) |> 
            map2(.y = r,
                 ~ mutate(.x,
                          r = .y |>  as.factor())) |>  
            map_dfr(rename,
                    x = 1, y = 2)) |> 
  ggplot() +
  aes(x = x, y = y) +
  geom_point() +
  facet_wrap( ~ r,
              nrow   = 3, 
              scales = "free")