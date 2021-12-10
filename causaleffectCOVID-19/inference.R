library(dplyr)
library(ggplot2)
library(CausalImpact)
df <- read.csv("Michigan (16).csv")
time.points <- as.Date(df$time_value)
df <- df %>% 
  dplyr::select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-06-28"))
post.period <- as.Date(c("2021-07-5", "2021-08-30"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))

impact.plot <- plot(impact) +
  labs(
    title = "Michigan Lifting Mask Mandate Effect",
    x = "Date",
    colour = "Gears"
  ) + theme_grey()
plot(impact.plot)

summary(impact)
summary(impact,"report")

sig.plot <- plot(impact$model$bsts.model, "coefficients")
plot(sig.plot)
summary(impact$model$bsts.model)


#Illinois
df <- read.csv("Illinois (9).csv")
time.points <- as.Date(df$time_value)
df <- df %>% 
  dplyr::select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-06-21"))
post.period <- as.Date(c("2021-06-28", "2021-08-23"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))

impact.plot <- plot(impact) +
  labs(
    title = "Illinois Lifting Mask Mandate Effect",
    x = "Date",
    colour = "Gears"
  ) + theme_grey()

plot(impact.plot)

summary(impact)
summary(impact,"report")

sig.plot <- plot(impact$model$bsts.model, "coefficients")
plot(sig.plot)
summary(impact$model$bsts.model)

#Texas
df <- read.csv("Texas (6).csv")
time.points <- as.Date(df$time_value)
df <- df %>% dplyr::select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-03-15"))
post.period <- as.Date(c("2021-03-22", "2021-05-17"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))


library(ggplot2)
impact.plot <- plot(impact) +
  labs(
    title = "Texas Lifting Mask Mandate Effect",
    x = "Date",
    colour = "Gears"
  ) + theme_grey()

plot(impact.plot)

summary(impact)
summary(impact,"report")

sig.plot <- plot(impact$model$bsts.model, "coefficients")
plot(sig.plot)
summary(impact$model$bsts.model)


#Illinois reenforcing
df <- read.csv("Illinois_enf.csv")
time.points <- as.Date(df$time_value)
df <- df %>% 
  dplyr::select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-09-06"))
post.period <- as.Date(c("2021-09-13", "2021-11-08"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))
library(ggplot2)
impact.plot <- plot(impact) +
  labs(
    title = "Illinois Re-enforcing Mask Mandate Effect",
    x = "Date",
    colour = "Gears"
  ) + theme_grey()

plot(impact.plot)

summary(impact)
summary(impact,"report")

sig.plot <- plot(impact$model$bsts.model, "coefficients")
plot(sig.plot)
summary(impact$model$bsts.model)


#Louisiana
df <- read.csv("Louisiana_enf.csv")
time.points <- as.Date(df$time_value)
df <- df %>% dplyr::select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-08-09"))
post.period <- as.Date(c("2021-08-16", "2021-10-11"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))


library(ggplot2)
impact.plot <- plot(impact) +
  labs(
    title = "Louisiana Re-enforcing Mask Mandate Effect",
    x = "Date",
    colour = "Gears"
  ) + theme_grey()

plot(impact.plot)

summary(impact)
summary(impact,"report")

sig.plot <- plot(impact$model$bsts.model, "coefficients")
plot(sig.plot)
summary(impact$model$bsts.model)


#Oregon
df <- read.csv("Oregon_enf (1).csv")
time.points <- as.Date(df$time_value)
df <- df %>% dplyr::select(-c(time_value,Illinois, Louisiana))#Mississippi,Hawaii,Kentucky
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-08-23"))
post.period <- as.Date(c("2021-08-30", "2021-10-25"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))


library(ggplot2)
impact.plot <- plot(impact) +
  labs(
    title = "Oregon Re-enforcing Mask Mandate Effect",
    x = "Date",
    colour = "Gears"
  ) + theme_grey()

plot(impact.plot)

summary(impact)
summary(impact,"report")


sig.plot <- plot(impact$model$bsts.model, "coefficients")
plot(sig.plot)
summary(impact$model$bsts.model)








df <- read.csv("Pennsylvania.csv")
time.points <- as.Date(df$time_value)
df <- df %>% 
  select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-07-12"))
post.period <- as.Date(c("2021-07-19", "2021-09-06"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))
plot(impact)
summary(impact)
summary(impact,"report")


# Iowa
df <- read.csv("Iowa (1).csv")
time.points <- as.Date(df$time_value)
df <- df %>% 
  select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2021-02-15"))
post.period <- as.Date(c("2021-02-22", "2021-04-19"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.01))
plot(impact)
summary(impact)
summary(impact,"report")


# Missisippi
df <- read.csv("Missisippi (1).csv")
df$time_value
df$time_value <- as.Date(df$time_value)
#df <- df %>% filter(time_value >= '2020-03-16',time_value <= '2020-12-07')
time.points <- as.Date(df$time_value)
df <- df %>% 
  select(-time_value)
data <- zoo(df, time.points)
str(data)

pre.period <- as.Date(c("2020-03-16", "2020-10-05"))
post.period <- as.Date(c("2020-10-12", "2020-12-07"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(
  niter=1000,prior.level.sd=0.1))
plot(impact)
summary(impact)
summary(impact,"report")


