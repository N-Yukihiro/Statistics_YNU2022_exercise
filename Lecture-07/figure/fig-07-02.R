library(pacman)
p_load(withr, purrr, dplyr, magrittr, ggplot2)

pop <- with_seed(2718,
                 rnorm(n = 1000) |> 
                   as_tibble())
pop_var <- pop %$% 
  subtract(value,
           value |> 
             mean()) |>  
  raise_to_power(2) |> 
  mean()
pop_mean <- pop %$% 
  mean(value)

#サイズ100で命中率95%
with_seed(2718,
          rerun(100,
                slice_sample(pop,
                             n = 130)) |> 
            map_dfr(summarise, 
                    lowerCI = add(
                      value |>  mean(),
                      qnorm(0.025) |>  
                        multiply_by(
                          divide_by(pop_var,
                                    value |>  
                                      length()) |>  
                            sqrt()
                        )),
                    mean = mean(value),
                    upperCI = add(
                      value |> mean(),
                      qnorm(0.975) |> 
                        multiply_by(
                          divide_by(pop_var,
                                    value |>  
                                      length()) |>  
                            sqrt()
                        )),
                    .id = "number")) |> 
  mutate(TF = if_else(
    lowerCI <= pop_mean & upperCI >= pop_mean,
    1, 0)) |> 
  mutate(across(TF, as.factor)) |> 
  ggplot() +
  aes(x = number, y = mean, colour = TF) +
  geom_point() +
  geom_errorbar(aes(ymin = lowerCI,
                    ymax = upperCI), 
                width = .2) +
  geom_hline(yintercept = pop_mean, 
             linetype   = "dashed", 
             colour     = "black") +
  theme_void()