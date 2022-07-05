library(pacman)
p_load(withr, dplyr, ggplot2)

dat <- with_seed(2718,
                 data.frame(x = rnorm(15, mean = 5)) |> 
                   mutate(y = 2 * x + rnorm(15),
                          c = sample(1:3, 
                                     size    = 15,
                                     replace = TRUE) |> 
                            as.factor()))
dat |> 
  ggplot() +
  aes(x = x, y = y) +
  geom_smooth(method      = "lm",
              se          = FALSE,
              alpha       = 0.1,
              colour      = "orange",
              show.legend = FALSE) +
  geom_smooth(aes(colour  = c),
              method      = "lm",
              se          = FALSE,
              fullrange   = TRUE,
              show.legend = FALSE,
              alpha       = 0.1) +
  geom_point() +
  scale_colour_manual(values = c("red", "green", "blue")) +
  theme_classic()