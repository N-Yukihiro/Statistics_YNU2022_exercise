library(pacman)
p_load(dplyr, ggplot2, tidyr)

data.frame(
  a = seq(from=0, to = 10, length.out = 1000)
) %>% 
  add_row(a  = c(0, 10)) %>% 
  mutate(b   = 10 - a,
         M_A = (a + b) / 2,
         M_G = sqrt(a * b),
         M_H = (2 * a * b) / (a + b),
         RMS = sqrt((a^2 + b^2) / 2)) %>% 
  select(-b) %>% 
  pivot_longer(col       = -a, 
               names_to  = "mean", 
               values_to = "amount") %>% 
  mutate(across(mean, as.factor)) %>% 
  ggplot(aes(a, amount, colour = mean)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('green', 'red', 'blue', 'yellow'),
                     breaks = c('RMS', 'M_A', 'M_G', 'M_H'),
                     labels = c("RMS",
                                bquote(bar(x)), 
                                expression(M[G]), 
                                expression(M[H]))) +
  labs(y = "aとbの平均値") +
  theme_bw(base_family = "IPAexGothic",base_size = 20) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
