library(ggplot2)
library(tidyr)

# Normal
df <- data.frame(x=numeric(), p=character(), y=numeric())

domain <- seq(-4, 4, length=100)
params <- c(1, 2, 4)

for (i in 1:length(params)){
  df <- rbind(df, data.frame(
    x=domain,
    p=rep(paste("stdev =", params[i])),
    y=dnorm(domain, mean=0, sd=params[i])))
}

ggplot(df, aes(x=x, y=y, color=p)) +
  geom_line() +
  ggtitle("The Normal Distribution (with mean=0)") +
  labs(x="x value", y="Density", color="Parameters")

ggsave("images/distribution-normal.png")

# Student's t
df <- data.frame(x=numeric(), p=character(), y=numeric())

domain <- seq(-4, 4, length=100)
params <- c(1, 2, 4, 8, 16)

for (i in 1:length(params)){
  df <- rbind(df, data.frame(
    x=domain,
    p=rep(paste("df =", params[i])),
    y=dt(domain, df=params[i])))
}

ggplot(df, aes(x=x, y=y, color=p)) +
  geom_line() +
  ggtitle("Student's t Distribution") +
  labs(x="x value", y="Density", color="Parameters")

ggsave("images/distribution-t.png")

# Poisson
df <- data.frame(x=numeric(), p=character(), y=numeric())

domain <- seq(0, 20, length=21)
params <- c(1, 2, 4, 8, 16)

for (i in 1:length(params)){
  df <- rbind(df, data.frame(
    x=domain,
    p=rep(paste("df =", params[i])),
    y=dpois(domain, lambda=params[i])))
}

ggplot(df, aes(x=x, y=y, color=p)) +
  geom_line() +
  geom_point() +
  ggtitle("Poisson Distribution") +
  labs(x="x value", y="Density", color="Parameters")

ggsave("images/distribution-poisson.png")

# Chi-square
df <- data.frame(x=numeric(), p=character(), y=numeric())

domain <- seq(0, 20, length=21)
params <- c(1, 2, 4, 8, 16)

for (i in 1:length(params)){
  df <- rbind(df, data.frame(
    x=domain,
    p=rep(paste("df =", params[i])),
    y=dchisq(domain, df=params[i])))
}

ggplot(df, aes(x=x, y=y, color=p)) +
  geom_line() +
  geom_point() +
  ggtitle("Chi-square Distribution") +
  labs(x="x value", y="Density", color="Parameters")

ggsave("images/distribution-chi-square.png")