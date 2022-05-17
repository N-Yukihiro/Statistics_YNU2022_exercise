library(pacman)
pacman::p_load(tibble, forcats, ggplot2)

par <- tibble(df = 1:9)

ggplot(data = NULL) + 
  aes(xmin = 0, xmax = 15) +
  mapply(
    function(df, co){
      stat_function(fun  = dchisq,
                    args = list(df = df),
                    aes_q(color = co))
    },
    par$df, 
    sprintf("df=%.0f", par$df |> 
              as_factor())) +
  xlab("") +
  ylab("")