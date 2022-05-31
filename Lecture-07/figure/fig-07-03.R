library(pacman)
p_load(withr, purrr, dplyr, magrittr, ggplot2)

pop <- with_seed(2718,
                 rnorm(n = 10000) |> 
                   as_tibble())
pop_var <- pop %$% 
  subtract(value,
           value |> 
             mean()) |> 
  raise_to_power(2) |> 
  mean()
pop_mean <- pop %$% 
  mean(value)

with_seed(2718,
          c(30, 100, 1000) |> 
            map(~ rerun(100, 
                        slice_sample(pop,
                                     n = .x))) |> 
            flatten() |> 
            map_dfr(mutate, 
                    size = n(), 
                    .id  = "number") |> 
            group_by(size, number) |> 
            summarise(
              lowerCI = add(
                value |> mean(),
                qnorm(0.025) |>  
                  multiply_by(
                    divide_by(pop_var,
                              value |>  length()) |>  
                      sqrt()
                  )),
              mean = mean(value),
              upperCI = add(
                value |>  mean(),
                qnorm(0.975) |>  
                  multiply_by(
                    divide_by(pop_var,
                              value |>  length()) |> 
                      sqrt()
                  ))) |> 
            mutate(number = row_number()) |> 
            ungroup()) |> 
  mutate(TF = if_else(
    lowerCI <= pop_mean & upperCI >= pop_mean,
    1, 0)) |> 
  mutate(across(TF,   as.factor)) |> 
  mutate(across(size, as.factor)) |>  
  ggplot() +
  aes(x      = number,
      y      = mean,
      colour = TF) +
  geom_point() +
  geom_errorbar(aes(ymin = lowerCI,
                    ymax = upperCI), 
                width = .2) +
  geom_hline(yintercept = pop_mean, 
             linetype   = "dashed", 
             colour     = "black") +
  theme_void() +
  facet_wrap(~size)