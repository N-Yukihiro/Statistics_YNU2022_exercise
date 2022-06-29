library(pacman)
p_load(ggdag)
dagify(x ~ z,
       y ~ z,
       y ~ x) |>  
  ggdag() +
  theme_void()