library(pacman)
p_load(tidyr, dplyr, seewave)

iris %>% 
  select(Sepal.Length) %>% 
  mutate(z1 = (Sepal.Length - mean(Sepal.Length)) 
         / rms(Sepal.Length - mean(Sepal.Length)),
         z2 = (Sepal.Length - mean(Sepal.Length))
         / sd(Sepal.Length)) %>% 
  pivot_longer(cols      = everything(), 
               names_to  = "var",
               values_to = "value") %>% 
  ggplot() +
  aes(x = value,
      y = ..density..) +
  geom_histogram() +
  geom_density() +
  facet_wrap(~var, 
             scales = "free",
             ncol   = 1,
             labeller = label_bquote(rows = "")) +
  theme_void()
