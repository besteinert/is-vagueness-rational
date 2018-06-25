library(rwebppl)
library(ggplot2)

tall <- webppl(program_file = "wppl-files/L1-tall.wppl")
short <- webppl(program_file = "wppl-files/L1-short.wppl")
not_tall <-webppl(program_file = "wppl-files/L1-not-tall.wppl")
not_short <- webppl(program_file = "wppl-files/L1-not-short.wppl")

xvalues=data.frame(x=c(-3, 3))
plot <- ggplot(xvalues, aes(x=xvalues)) +  ggtitle("Posteriors of inferred worldstates, type=[mean=0.8, sd=0.01, alpha=50]") + xlab("worldstate")+ ylab("P(w|m)") +
geom_area(data=tall, aes(x=support, y=prob,fill = "tall"), color= "mediumpurple3", alpha=0.6)+
geom_area(data=short, aes(x=support, y=prob, fill= "short"),color="dodgerblue3", alpha=0.6) +
geom_area(data=not_short, aes(x=support, y=prob, fill="not-short"),color="tomato3", alpha=0.6) +
geom_area(data=not_tall, aes(x=support, y=prob, fill = "not-tall"),color="olivedrab4", alpha=0.6) + 
#stat_function(fun=dnorm, color="black") + annotate("text", x=1.25, y=0.3, size=6,label="Prior")+
geom_vline(xintercept = 0.8, linetype="dashed") + geom_vline(xintercept = -0.8, linetype="dashed") + 
scale_fill_discrete(name="Message") + theme_bw()
plot
 
