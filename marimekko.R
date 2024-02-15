## https://learnr.wordpress.com/2009/03/29/ggplot2-variable-width-column-chart/
## https://www.r-graph-gallery.com/81-barplot-with-variable-width.html
## https://www.yan-holtz.com/
library(ggplot2)

df <- data.frame(x = c("Alpha", "Beta", "Gamma",
     "Delta"), width = c(25, 250, 75, 100), height = c(100,
     75, 50, 25))

df$w <- cumsum(df$width)
df$wm <- df$w - df$width
df$wt <- with(df, wm + (w - wm)/2)


p  <- ggplot(df, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = height, fill = x))

p2 <- p1 + geom_text(aes(x = wt, y = height * 0.5, label = x))

p3 <- p2 + theme_bw() + 
     ###opts(legend.position = "none") +
     labs(x = NULL, y = NULL)

ggsave("marimeko_01.png", plot=p1)
ggsave("marimeko_02.png", plot=p2)
ggsave("marimeko_03.png", plot=p3)
