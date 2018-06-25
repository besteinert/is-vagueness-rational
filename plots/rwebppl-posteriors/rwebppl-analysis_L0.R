library(rwebppl)
library(ggplot2)

tall <- webppl(program_file = "wppl-files/L0-tall.wppl")


tall$prob = tall$prob*100

xvalues=data.frame(x=c(-2, 2))
plot <- ggplot(xvalues, aes(x=xvalues)) +  ggtitle("Prior and Posterior of inferred worldstates of L0") + xlab("worldstate")+ ylab("probability") +
geom_area(data=tall, aes(x=support, y=prob, fill = "tall"), color= "mediumpurple3", alpha=0.6)+ theme_bw() +
stat_function(fun=dnorm, color="black") + 
annotate("text", x=-1.4, y=0.35, size=6,label="worldstate-\n Prior")+
geom_vline(xintercept = 1.5, linetype="dashed") + annotate("text", x=1.6, y=0.3, size=6, label="theta=1.5")+
scale_fill_discrete(name="Message") 
plot 
