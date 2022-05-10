library(pacman)
pacman::p_load(ggplot2)

ggplot(data = NULL) +
  aes(xmin = -4, 
      xmax = 4) +
  stat_function(fun = dnorm) +
  geom_ribbon(
    data = data.frame(
      X = x <- seq(-2, 2, len=1000),
      Y = dnorm(x)),
    aes(x    = X,
        y    = Y,
        ymin = 0,
        ymax = Y),
    fill = "#001E62") +
  geom_ribbon(
    data = data.frame(
      X = x <- seq(-1, 1, len=1000),
      Y = dnorm(x)),
    aes(x    = X, 
        y    = Y,
        ymin = 0,
        ymax = Y),
    fill = "#C63527") +
  xlab("") +
  ylab("")