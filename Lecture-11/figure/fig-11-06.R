library(pacman)
p_load(dplyr, withr, gridExtra)
with_seed(2718,
          data.frame(z = sample(1:5, 
                                size    = 100, 
                                replace = TRUE)) |> 
            mutate(x = rnorm(n = 100, sd = 0.3) + z,
                   y = rnorm(n = 100, sd = 0.3,
                             mean = -0.7 * x) + z,
                   z = as.factor(z))) %>% 
  {
    {
      p1 <<- ggplot(.) +
        aes(x = x, y = y) +
        geom_point()
    }
    {
      p2 <<- ggplot(.) +
        aes(x = x, y = y, colour = z) +
        geom_point(show.legend = FALSE)
    }
    {
      p3 <<- ggplot(.) +
        aes(x = x, y = y, colour = z) +
        geom_point(show.legend = FALSE) +
        geom_smooth(method = "lm", formula = y ~ x, 
                    se = FALSE, show.legend = FALSE) +
        facet_wrap(~ z, nrow = 1, scales = "free")
    }
  }

gridExtra::grid.arrange(p1, p2, p3, 
                        layout_matrix = matrix(c(1, 3, 2, 3), 
                                               ncol = 2))