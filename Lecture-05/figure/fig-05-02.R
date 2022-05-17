library(pacman)
pacman::p_load(dplyr, purrr, magrittr, tidyr, withr, ggplot2)

size <- seq(from = 5,
            to   = 52,
            by   = 4)

monte <- with_seed(2718, 
                   map(.x = size,
                       .f = ~ data.frame(
                         x = runif(.x^2, min = 0, max = 1),
                         y = runif(.x^2, min = 0, max = 1)
                       )
                   ) |>  
                     map(mutate, 
                         r = if_else(
                           x^2 + y^2 <= 1,
                           1,
                           0)
                     ) |> 
                     map_dfr(summarise,
                             pi = mean(r) * 4, size = n()))

syst <- map(.x = size,
            .f = ~seq(from       = 0,
                      to         = 1,
                      length.out = .x)) |>  
  map(~expand_grid(.x, .x)) |> 
  map(as_tibble) |> 
  map(set_colnames, c("x", "y")) |> 
  map(mutate,
      r = if_else(
        x^2 + y^2 <= 1,
        1,
        0)
  ) |> 
  map_dfr(summarise, 
          pi = mean(r) * 4, 
          size = n())

tate <- function(x){
  chartr("ー", "丨", x) |> # 長音符の処理
    strsplit(split = "") |>  
    sapply(paste, collapse = "\n")
}

ggplot() +
  geom_point(
    data   = monte,
    aes(x  = size,
        y  = pi),
    colour = "red") +
  geom_point(
    data   = syst,
    aes(x  = size, 
        y  = pi),
    colour = "blue") +
  geom_hline(yintercept = pi) +
  ylab("円周率" |> tate()) +
  xlab("点の数") +
  theme_classic(base_size   = 20,
                base_family = "IPAexGothic") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))