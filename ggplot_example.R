library(ggplot2)
dataset <- data.frame(
  x = rep(c(1:5), 3),
  y = sample(1:15),
  group = as.factor(rep(c(1:3), 5))
)


ggplot(data=dataset, aes(x=x, y=y, group=group, col=group)) + geom_line()
