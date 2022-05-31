library(pacman)
p_load(ggplot2, withr)

X <- with_seed(2718,
               rnorm(20))

logLikelihood <- function(args, x){
  likelihood <- 1 / sqrt(2 * pi * args[2] ^ 2) * exp(-0.5 * ((x - args[1]) / args[2]) ^ 2)
  return(sum(log(likelihood)))
}

mle <- optim(par     = c(0,1),
             fn      = logLikelihood,
             x       = X,
             control = list(fnscale = -1))$par

d <- data.frame(x    = X,
                y    = 0,
                xend = X, 
                yend = dnorm(X, 
                             mean = mle[1],
                             sd   = mle[2]))

ggplot() +
  geom_rug(data  = NULL,
           aes(x = X),
           sides = "b") +
  geom_segment(data     = d,
               aes(x    = x,
                   y    = y,
                   xend = xend,
                   yend = yend),
               linetype = "dotted",
               size     = 0.25) +
  stat_function(data = NULL, 
                aes(xmin = -4,
                    xmax = 4),
                fun  = dnorm,
                args = list(mean = mle[1], 
                            sd   = mle[2]),
                size = 2) +
  stat_function(data = NULL,
                fun  = dnorm,
                args = list(mean = mle[1] + 1, 
                            sd   = mle[2]),
                colour = "gray") +
  stat_function(data = NULL,
                fun  = dnorm,
                args = list(mean = mle[1] - 1, 
                            sd   = mle[2]),
                colour = "gray")  +
  stat_function(data = NULL,
                fun  = dnorm,
                args = list(mean = mle[1], 
                            sd   = mle[2] * 1.25),
                colour = "gray") +
  stat_function(data = NULL,
                fun  = dnorm,
                args = list(mean = mle[1], 
                            sd   = mle[2] * 0.75),
                colour = "gray") +
  theme_bw()