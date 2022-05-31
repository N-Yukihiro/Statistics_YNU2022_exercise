library(pacman)
p_load(ggplot2, dplyr)
ggplot(data = NULL) +
  stat_function(fun  = dt,
                args = list(df = 20)) +
  geom_ribbon(
    data = tibble(
      X = seq(-4, 
              qt(p   = 0.025,
                 df  = 20),
              len = 1000),
      Y = dt(X, 
             df = 20)
    ),
    aes(x    = X, 
        y    = Y, 
        ymin = 0, 
        ymax = Y),
    fill = "red") +
  geom_ribbon(
    data = tibble(
      X = seq(qt(p          = 0.975, 
                 df         = 20, 
                 lower.tail = TRUE),
              4, 
              len = 1000),
      Y = dt(X, 
             df = 20)),
    aes(x    = X,
        y    = Y,
        ymin = 0,
        ymax = Y),
    fill = "red") +
  xlab("") +
  ylab("") +
  theme_bw()