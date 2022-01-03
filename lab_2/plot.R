# Create Data
perda <- sample - sum(data$value)
sample <- 1.998

data <- data.frame(
  group=c("Naftaleno", "Sal", "Areia", "Perdas"),
  value=c(0.334, 0.457, 1.204, perda)
)


x <-  data$value
labels <-  c("London","New York","Singapore","Mumbai")
# calculando a proporção
data$percent<- round(100*x/sum(x), 1)

# Plot the chart.
pie(x, labels = piepercent, main = "Proporção dos componentes da amostra [%]",
    col = RColorBrewer::brewer.pal(n = 4,
                                   name = "Set3"))
legend("topright", 
       data$group, 
       cex = 1,
       fill = RColorBrewer::brewer.pal(n = 4,
                                       name = "Set3"))

bp <- ggplot(data = data, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie <- bp + coord_polar("y", start=0)

pie + scale_fill_brewer(palette = "YlOrRd") +blank_theme+
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent), size=6,
            col = "Black")

