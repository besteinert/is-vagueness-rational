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
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


x <- seq(0,3,length=300)

### Graph a)
ylab_a = "P(x is tall)"
dens_crisp <- pnorm(x,mean=1.5, sd=0.001)
graph_a <- ggplot()  + geom_area(aes(x=x, y=dens_crisp),  fill="lightblue3") + 
  geom_step(aes(x=x, y=dens_crisp)) + annotate("text", x=0.7, y=0.7, label= "not-tall") + 
  annotate("text", x=2.2, y=0.7, label= "tall") + 
  theme_bw() +
  scale_y_continuous(name=ylab_a) + scale_x_continuous(name="")

### Graph b)
ylab_b = "P(x is tall)"
dens_vag <- pnorm(x,mean=1.5, sd=0.2)
graph_b <- ggplot()  + geom_area(aes(x=x, y=dens_vag),  fill="lightblue3") + 
  geom_step(aes(x=x, y=dens_vag)) + geom_vline(xintercept = 2.2) + geom_vline(xintercept = 0.8) +
  theme_bw() + annotate("text", x=0.4, y=0.7, label= "not-tall") + annotate("text", x=1.5, y=0.7, label= "???") + 
  annotate("text", x=2.5, y=0.7, label= "tall") + scale_y_continuous(name=ylab_b)  + scale_x_continuous(name="")

### Graph c)
ylab_c = expression(paste("P(", theta, ")"))
p_crisp <- dnorm(x,mean=1.5, sd=0.0015)
graph_c <- ggplot()  + geom_area(aes(x=x, y=p_crisp),  fill="lightblue3") + 
  geom_step(aes(x=x, y=p_crisp)) + 
  theme_bw() +
  scale_y_continuous(name=ylab_c, labels = NULL) 

### Graph d)
ylab_d = expression(paste("P(", theta, ")"))
p_vag <- dnorm(x,mean=1.5, sd=0.2)
graph_d <- ggplot()  + geom_area(aes(x=x, y=p_vag),  fill="lightblue3") + 
  geom_step(aes(x=x, y=p_vag)) + 
  theme_bw() +
  scale_y_continuous(name=ylab_d, labels = NULL)


multiplot(graph_a, graph_c, graph_b, graph_d, cols=2)
