library(ggplot2)
library(gganimate)
library(ggdark)

theme_set(dark_theme_grey())

RealiseWiener <- function() {
  w <- 0
  for (i in 1:400) {
    w[i + 1] <- w[i] + rnorm(1, mean = 0, sd = sqrt(0.01))
  }
  w
}

GBM <- 10*exp(RealiseWiener())

time <- seq(from= 0, to = 4, by = 0.01)

df <- data.frame(time = time, price = GBM, realisation = 1, final = GBM[length(GBM)])

for (n in 2:50) {
  GBM <- 10*exp(RealiseWiener())
  df <- rbind(df, data.frame(time = time, price = GBM, realisation = n, final = GBM[length(GBM)]))
}

df$realisation <- as.factor(df$realisation)

gg <- ggplot(data = df) +
  geom_line(aes(x=time, y= price, group = realisation, colour = final),
            alpha = 0.8) +
  labs(title = "50 realisations of Geometric Brownian Motion: modelling share prices",
       colour = "Final price")

anim <- gg + transition_reveal(time)

animate(anim, renderer = ffmpeg_renderer(), height = 725, width = 1024, end_pause = 30)

