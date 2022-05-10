library(pacman)
pacman::p_load(dplyr, ggplot2)

tate <- function(x){
  chartr("ー", "丨", x) |> # 長音符の処理
    strsplit(split = "") |> 
    sapply(paste, collapse="\n")
}

t_p <- c(.49, .55, .6)
seq(from = 1, to = 301, by = 2) |> 
  rep(3) |> 
  tibble(n = _) |> 
  mutate(t_p = rep(t_p,
                   each = length(n) / length(t_p)),
         probs = 1 - pbinom(q    = floor(n / 2),
                            size = n, 
                            prob = t_p)
  ) |> 
  mutate(across(t_p, as.character)) |> 
  ggplot() +
  aes(x      = n,
      y      = probs,
      colour = t_p) +
  geom_line() +
  geom_hline(yintercept = .8,
             colour     = "black") +
  geom_hline(yintercept = .5,
             linetype   = "dashed",
             colour     = "black") +
  guides(colour = guide_legend(reverse = TRUE)) +
  xlab("多数決に参加する人数") +
  ylab("多数決の結果が正しい確率" |> tate()) +
  labs(colour = "個人の選択が\n正しい確率") +
  theme_bw(base_size   = 20,
           base_family = "IPAexGothic") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))
