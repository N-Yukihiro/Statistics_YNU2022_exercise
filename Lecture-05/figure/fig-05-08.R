library(pacman)
pacman::p_load(dplyr, tibble, purrr, withr, ggplot2)

var   <- 4
size2 <- 100
with_seed(2718,
          rerun(1000, 
                sample_n(rnorm(n  = 100000, 
                               sd = sqrt(var)) |> 
                           as_tibble(),
                         size     = size2, 
                         replace  = FALSE)) |> 
            map_dfr(summarise, 
                    unbiased_var = var(value)) |> 
            mutate(z = ((size2 - 1)/ var) * unbiased_var)) |> 
  ggplot() + 
  aes(z) +
  geom_histogram(aes(y = ..density..),
                 alpha       = 0.1,
                 fill        = "blue",
                 show.legend = FALSE) +
  geom_density(fill   = "blue",
               colour = "blue",
               alpha  = 0.2) +
  geom_vline(xintercept = size2 -1,
             colour     = "red") +
  stat_function(fun  = dchisq,
                args = list(df = size2 -1),
                size = 1.5) +
  stat_function(fun      = dnorm,
                args     = list(mean = size2 -1,
                                sd   = sqrt(2 * (size2 - 1))),
                linetype = "dashed") +
  theme_bw(base_size   = 20,
           base_family = "IPAexGothic")