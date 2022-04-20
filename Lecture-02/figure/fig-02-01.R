## 夕焼け
pacman::p_load(ggplot2)
ggplot(data = NULL) +
  aes(xmin = -4, xmax = 4) +
  geom_point(aes(x = 1,
                 y = 0.3),
             size   = 25,
             colour = "yellow") +
  stat_function(fun  = dnorm,
                args = list(sd = 1.5),
                aes(ymin = 0, 
                    ymax = ..y..),
                geom = "ribbon",
                fill = "blue") +
  theme_void() +
  theme(panel.background = 
          element_rect(fill = "orange"))
