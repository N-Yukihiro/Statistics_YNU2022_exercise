library(pacman)
p_load(ggplot2, dplyr)
bind_rows(
  tibble(x    = 0:4,
         y    = dbinom(x    = 0:4, 
                       size = 4,
                       prob = 0.5),
         prob = "50%"),
  tibble(x    = 0:4,
         y    = dbinom(x    = 0:4, 
                       size = 4,
                       prob = 0.75),
         prob = "75%")) |> 
  ggplot(data = _) +
  aes(x      = x,
      y      = y,
      colour = prob) +
  geom_path() +
  geom_vline(xintercept = 3,
             linetype   = "dashed") +
  xlab("") +
  ylab("") +
  theme_bw()