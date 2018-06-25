library(ggplot2)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

x <- seq(-3,3,length=600)


###Vague semantics of tall and not-tall
ylab_b = "P(m is true)"

lit_tall <- pnorm(x,mean=1, sd=0.3)
lit_not_tall <- 1-pnorm(x,mean=1, sd=0.3)
th_prior <- dnorm(x,mean=1, sd=0.3)
graph_b <- ggplot()  + geom_line(aes(x=x, y=th_prior)) +
  geom_area(aes(x=x, y=lit_tall, colour="tall"),  fill="lightblue3", alpha = 0.6) + 
  geom_area(aes(x=x, y=lit_not_tall, colour="not-tall"),  fill="firebrick", alpha = 0.3) +
  scale_colour_hue(name="Message") + theme_bw() + 
  annotate("text", x=0.3, y=1.25, label="Pr(theta)" ) + 
  scale_y_continuous(name=ylab_b)  + scale_x_continuous(name="w") 

ggsave("vague-semantics.pdf")
