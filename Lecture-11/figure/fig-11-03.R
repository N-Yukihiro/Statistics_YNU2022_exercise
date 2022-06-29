library(dplyr)
library(ggplot2)

circle <- data.frame(t = seq(from       = 0, 
                             to         = 2*pi,
                             length.out = 100)) |> 
  mutate(x = cos(t),
         y = sin(t)) |> 
  dplyr::select(-t) |> 
  mutate(type = factor("circle"))

quadra <- data.frame(x = seq(from       = -1,
                             to         = 1,
                             length.out = 100)) |> 
  mutate(y = x^2 -0.5) |> 
  mutate(type = factor("quadratic"))

sin <- data.frame(x = seq(from       = -1.5 * pi, 
                          to         = 2.5  * pi, 
                          length.out = 100)) |> 
  mutate(y = sin(x)) |>  
  mutate(type = factor("sin"))

bind_rows(circle, quadra, sin) |> 
  ggplot() +
  aes(x = x, y = y) +
  geom_point() +
  facet_wrap(~ type, ncol = 3, scale = "free") +
  xlab("") +
  ylab("") +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())