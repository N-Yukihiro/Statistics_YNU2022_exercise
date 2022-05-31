library(pacman)
p_load(ggplot2, dplyr)

bind_rows(
  tibble(x    = 0:20,
         y    = dbinom(x    = 0:20,
                       size = 20,
                       prob = 0.5),
         prob = "50%"),
  data.frame(x    = 0:20,
             y    = dbinom(x    = 0:20,
                           size = 20,
                           prob = 0.75),
             prob = "75%")) |> 
  ggplot(data = _) +
  aes(x      = x, 
      y      = y,
      colour = prob) +
  geom_path() +
  geom_vline(xintercept = 15,
             linetype   = "dashed") +
  xlab("") +
  ylab("") +
  theme_bw()