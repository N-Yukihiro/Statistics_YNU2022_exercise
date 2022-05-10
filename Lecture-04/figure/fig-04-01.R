library(pacman)
pacman::p_load(dplyr, tibble, ggplot2)

tate <- function(x){
  chartr("ー", "丨", x) |>  # 長音符の処理
    strsplit(split = "") |> 
    sapply(paste, collapse="\n")
}

sense <- 0.8
spec <- 0.999

tibble(
  inf = seq(0.001, 0.20 ,by = 0.001)
  ) |> 
  mutate(pos = (sense * inf) / (sense * inf + (1 - spec) * (1 - inf)),
         neg = (spec * (1-inf)) / (spec * (1 - inf) + (1 - sense) * inf)) |> 
  ggplot(aes(x = inf)) +
  geom_line(aes(y = pos), colour = "red") +
  geom_line(aes(y = neg), colour = "blue") +
  ylab(tate("的中率")) +
  xlab("感染率") +
  theme_bw(base_size = 20, base_family = "IPAexGothic") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust= 0.5))