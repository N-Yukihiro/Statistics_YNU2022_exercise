## 夜景
pacman::p_load(ggplot2)
ggplot(data = NULL) +
  aes(xmin = -4, 
      xmax = 4) +
  geom_point(aes(x = 1, 
                 y = 0.4),
             size   = 25,
             colour = "yellow") +
  geom_point(data   = 
               data.frame(
                 x = runif(n   = 50,
                           min = -4,
                           max = 4),
                 y = runif(n   = 50,
                           min = 0.2,
                           max = 0.5)),
             aes(x = x, 
                 y = y),
             size   = 2,
             colour = "yellow") +
  stat_function(fun  = dnorm,
                args = list(sd = 1.5),
                aes(ymin = 0, 
                    ymax = ..y..),
                geom = "ribbon",
                fill = "darkblue") +
  theme_void() +
  theme(panel.background =
          element_rect(fill = "black"))