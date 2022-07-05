library(pacman)
p_load(withr, dplyr, tidyr, purrr, magrittr, ggplot2)

dat <- with_seed(2718,
                 data.frame(x = rnorm(15)) |> 
                   mutate(y = 2 * x + rnorm(15),
                          c = sample(1:3, 
                                     size    = 15,
                                     replace = TRUE) |> 
                            as.factor()))

tate <- function(x){
  chartr("ー", "丨",
         x) |>   # 長音符の処理
    strsplit(split  = "") |>  
    sapply(paste, 
           collapse = "\n")
}

dat |> 
  group_by(c) |> 
  nest() |> 
  mutate(c = as.numeric(c)) |> 
  bind_rows(dat |> 
              nest() |>  
              mutate(c = 0)) %>%
  mutate(lm = map(data,
                  ~lm(y ~ x, data = .))) |> 
  mutate(newx = dat$x |> list(),
         newy = dat$y |> list()) %>% 
  mutate(pred = map2(newx, lm,
                     ~predict(.y, 
                              data.frame(.x) |> 
                                set_names("x")))) %>% 
  transmute(diff = map2(newy, pred,
                        ~ subtract(.x, .y) |> 
                          raise_to_power(2) |>  
                          sum())) |> 
  unnest() |> 
  mutate(c = as.factor(c)) |> 
  ggplot() +
  aes(x = c, y = diff, fill = c) +
  geom_bar(stat        = "identity",
           show.legend = FALSE) +
  scale_fill_manual(values = c("orange", "red", "green", "blue")) +
  scale_x_discrete(labels = c("orange", "red", "green", "blue")) +
  xlab(NULL) +
  ylab(tate("直線と実測値の差の2乗"))  +
  theme_classic(base_size   = 20,
                base_family = "IPAexGothic") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5))