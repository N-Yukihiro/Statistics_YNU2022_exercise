library(pacman)
pacman::p_load(dplyr, tibble, purrr, withr, ggplot2)

var      <- 4
pop_mean <- 1
size2    <- 10

with_seed(2718,
          rerun(1000, 
                sample_n(rnorm(n    = 100000, 
                               mean = pop_mean,
                               sd   = sqrt(var)) |> 
                           as_tibble(),
                         size    = size2, 
                         replace = FALSE)) |> 
            map_dfr(summarise, 
                    sample_mean  = mean(value),
                    unbiased_var = var(value)) |> 
            mutate(z = (sqrt(size2) * (sample_mean - pop_mean)) / sqrt(unbiased_var))) |> 
  ggplot() + 
  aes(x = z) +
  geom_histogram(aes(y = ..density..),
                 alpha       = 0.1,
                 fill        = "blue",
                 show.legend = FALSE) +
  geom_density(fill   = "blue",
               colour = "blue",
               alpha  = 0.2) +
  geom_vline(xintercept = 0,
             colour     = "red") +
  stat_function(fun  = dt,
                args = list(df = size2 - 1),
                size = 1.5) +
  stat_function(fun      = dnorm,
                linetype = "dashed") +
  theme_bw(base_size   = 20,
           base_family = "IPAexGothic")