# パッケージの読み込み
library(pacman)
p_load(ggplot2, dplyr, tidyr, modeest, withr)

# グループごとの平均・中央・最頻値を定義
mean_by_group <- . %>% 
  group_by(var) %>% 
  summarize(mean_x = mean(value))
median_by_group <- . %>% 
  group_by(var) %>% 
  summarize(median_x = median(value))
mode <- data.frame(var = c("x1", "x2", "x3"),
                   value = c(mlv("normal", 
                                 mean   = 5, 
                                 sd     = 0.1),
                             mlv("beta",   
                                 shape1 = 10, 
                                 shape2 = 2),
                             mlv("beta",  
                                 shape1 = 2,
                                 shape2 = 10)))

# データの作成, グラフの描画
set.seed(2718)
withr::with_preserve_seed(
  data.frame(x1 = rnorm(n      = 100000,
                        mean   = 5,
                        sd     = 0.1),
             x2 = rbeta(n      = 100000, 
                        shape1 = 10, 
                        shape2 = 2),
             x3 = rbeta(n      = 100000,
                        shape1 = 2, 
                        shape2 = 10)) %>% 
    tidyr::pivot_longer(cols      = c(x1, x2, x3),
                        names_to  = "var",
                        values_to = "value") %>%
    ggplot() +
    aes(x = value,
        y = ..density..) +
    geom_density() +
    geom_vline(data   = mean_by_group, 
               aes(xintercept = mean_x), 
               colour = "red",
               size   = 1.) +
    geom_vline(data     = median_by_group, 
               aes(xintercept = median_x), 
               colour   = "black",
               linetype = "dashed",
               size = 0.7) +  
    geom_vline(data     = mode,
               aes(xintercept = value),
               colour   = "black",
               linetype = "twodash",
               size     = 0.5) +
    facet_wrap(~var,
               scales = "free", 
               ncol = 1,
               labeller = label_bquote(rows = "")) +
    theme_void()
)
