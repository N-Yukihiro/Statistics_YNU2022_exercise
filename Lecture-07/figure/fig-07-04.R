qt(p  = 0.025,
   df = 10:200) |> 
  as_tibble() |> 
  mutate(size = 10:200,
         diff = qnorm(0.025) - value) |> 
  ggplot() +
  aes(x = size,
      y = diff) +
  geom_path()