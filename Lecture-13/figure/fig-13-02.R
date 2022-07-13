library(pacman)
p_load(ggdag)
dagify(X ~ Z,
       Y ~ Z + X,
       exposure = "X",
       outcome  = "Y",
       coords   = list(
         x = c(X = 1, Y = 2, Z = 3),
         y = c(X = 2, Y = 1, Z = 2))) |> 
  tidy_dagitty() |> 
  ggdag_adjust(var = "Z") +
  theme_void() +
  theme(legend.position = "none")