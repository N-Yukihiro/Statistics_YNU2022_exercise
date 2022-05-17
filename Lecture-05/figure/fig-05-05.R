library(pacman)
pacman::p_load(tibble, dplyr, purrr, withr, forcats, ggplot2)

size1 <- 10
size2 <- 100
size3 <- 1000

with_seed(2718,
          list(size1, size2, size3) |> 
            map(~rerun(10000, 
                       data.frame(x = rbinom(n    = .x,
                                             size = 1, 
                                             prob = 0.5)))) |> 
            purrr::set_names(c("10", "100", "1000")) |> 
            map_dfr(map_dfr,
                    summarise, 
                    sum = sum(x),
                    .id  = "size")) |>  
  mutate(across(.cols = size,
                .fns  = as.numeric)) |> 
  mutate(z = (sum - size * 0.5) / sqrt(size * 0.5^2)) |>  
  mutate(across(.cols = size,
                .fns  = as.factor)) |>  
  ggplot() +
  aes(x      = z,
      fill   = size,
      colour = size) +
  geom_histogram(aes(y = ..density..),
                 alpha       = 0.1,
                 position    = "identity",
                 show.legend = TRUE) +
  geom_density(alpha       = 0,
               size        = 1.5,
               adjust      = 2.3,
               show.legend = FALSE) +
  stat_function(fun    = dnorm,
                colour = "black",
                size   = 1.1) +
  ylab("") +
  xlab("標準化された標本平均") +
  theme_bw(base_size   = 20,
           base_family = "IPAexGothic") +
  theme(axis.text.y = element_blank())