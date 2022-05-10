library(pacman)
pacman::p_load(dplyr, ggplot2, tidyr)

tate <- function(x){
  chartr("ー", "丨", x) |>  # 長音符の処理
    strsplit(split = "") |> 
    sapply(paste, collapse="\n")
}

expand_grid(x = 1:6, y = 1:6) |> 
  as_tibble() |> 
  ggplot() +
  aes(x = x,
      y = y) +
  geom_point() +
  scale_x_continuous(breaks = 1:6, 
                     name   = "1個目のサイコロの目") +
  scale_y_continuous(breaks = 1:6,
                     name   = "2個目のサイコロの目" |> tate()) +
  theme_classic(base_size   = 20, 
                base_family = "IPAexGothic") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))
