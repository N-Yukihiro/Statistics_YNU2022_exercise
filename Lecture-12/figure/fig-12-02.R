library(pacman)
p_load(withr, dplyr, ggplot2)

dat <- with_seed(2718,
                 data.frame(x = rnorm(15)) |>  
                   mutate(y = 2 * x + rnorm(15),
                          c = sample(1:3, 
                                     size    = 15,
                                     replace = TRUE) |> 
                            as.factor()))

l <- lm(y ~ x, data = dat)

dat |> 
  mutate(pred = predict(l)) |> 
  ggplot() +
  aes(x = x, y = y) +
  geom_smooth(method = "lm", 
              se     = FALSE,
              alpha  = 0.1,
              colour = "orange") +
  geom_point() +
  geom_linerange(aes(ymin = y, ymax = pred)) +
  theme_classic()