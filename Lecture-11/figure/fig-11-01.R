library(pacman)
p_load(withr, magrittr, MASS, dplyr, ggplot2, latex2exp)

text <- data.frame(
  x = rep(c(1, -1, -1, 1), 2),
  y = c(1.2, 1.2, -0.7, -0.7, 0.7, 0.7, -1.2, -1.2),
  text = c("A", "B", "C", "D", 
           TeX("$(x > \\bar{x}) \\bigcap (y > \\bar{y})$", 
               output = "character"),
           TeX("$(x < \\bar{x}) \\bigcap (y > \\bar{y})$", 
               output = "character"),
           TeX("$(x < \\bar{x}) \\bigcap (y < \\bar{y})$", 
               output = "character"),
           TeX("$(x > \\bar{x}) \\bigcap (y < \\bar{y})$", 
               output = "character")),
  colour = rep(c("red", "blue"),4)
)

with_seed(721,
          MASS::mvrnorm(n = 30, mu = c(0, 0), 
                        Sigma = matrix(c(1, 0.9, 0.9, 1),
                                       ncol = 2),
                        empirical = TRUE)) |> 
  as.data.frame() |> 
  rename(x = 1, y = 2) %$% 
  {
    ggplot(data = NULL) +
      aes(x = x, y = y) +
      geom_point(alpha = 0.5) +
      geom_vline(xintercept = mean(x),
                 linetype   = 2) +
      geom_hline(yintercept = mean(y), 
                 linetype   = 2) +
      scale_x_continuous(breaks = c(-1, 0, 1),
                         labels = c("-", expression(bar(x)), "+")) +
      scale_y_continuous(breaks = c(-1, 0, 1),
                         labels = c("-", expression(bar(y)), "+")) +
      geom_text(data = text,
                aes(x      = x, 
                    y      = y, 
                    label  = text,
                    colour = colour),
                size        = 10,
                parse       = TRUE,
                show.legend = FALSE) +
      theme_classic(base_size = 20)
  }