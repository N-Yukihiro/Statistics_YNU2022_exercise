# パッケージの読み込み
library(pacman)
p_load(ggplot2, dplyr, tidyr, modeest, withr)

# グループごとの平均値を定義
AM_by_group <- . %>% 
  group_by(var) %>% 
  summarize(AM_x = sum(value) / length(value))
GM_by_group <- . %>% 
  group_by(var) %>% 
  summarize(GM_x = exp(sum(log(value)) / length(value)))
HM_by_group <-   . %>% 
  group_by(var) %>% 
  summarize(HM_x = (sum(value^(-1)) / length(value))^(-1))
RMS_by_group <-    . %>% 
  group_by(var) %>% 
  summarize(RMS_x = sqrt(sum(value^2) / length(value)))

# データの作成, グラフの描画
set.seed(2718)
withr::with_preserve_seed(
  data.frame(x1 = rnorm(n      = 1000000,
                        mean   = 5, 
                        sd     = 1),
             x2 = rbeta(n      = 1000000, 
                        shape1 = 10, 
                        shape2 = 2),
             x3 = rbeta(n      = 1000000, 
                        shape1 = 2, 
                        shape2 = 10)) %>% 
    tidyr::pivot_longer(cols      = c(x1, x2, x3),
                        names_to  = "var",
                        values_to = "value") %>%
    ggplot() +
    aes(x = value,
        y = ..density..) +
    geom_density() +
    geom_vline(data   = AM_by_group, 
               aes(xintercept = AM_x), 
               colour = "red",
               size   = 1.) +
    geom_vline(data   = GM_by_group, 
               aes(xintercept = GM_x), 
               colour = "blue",
               size   = 0.7) +  
    geom_vline(data   = HM_by_group, 
               aes(xintercept = HM_x), 
               colour = "yellow",
               size   = 2) +  
    geom_vline(data   = RMS_by_group, 
               aes(xintercept = RMS_x), 
               colour = "green",
               size   = 0.7) +  
    facet_wrap(~var, 
               scales = "free",
               ncol = 1,
               labeller = label_bquote(rows = "")) +
    theme_void()
)
