library(tidyverse)

dados <- tibble(Vol_NaOH_pratico = c(15*10^-3, 14.8*10^-3, 14.9*10^-3),
                Vol_NaOH_teorico = c(13.025*10^-3,13.025*10^-3,13.025*10^-3))

ttest = t.test(x = dados$Vol_NaOH_pratico, y = dados$Vol_NaOH_teorico)

names(ttest)

boxplot(dados$Vol_NaOH_pratico,
        col = 'gray',
        main = "Volumes",
        xlab = "Obeservações",
        ylab = "Volume [L]",
        ylim = c(0.013025, 0.015))

abline(h=0.013025,
       col ="blue",
       lty = 5)
legend("right",
       legend = c("Volume teórico","Volume experimental"), lty = c(5,1),
       fill= "gray",
       col = c("blue", "black"),
       border = "White")
# Fellipe Mira Chaves e Tales A. Godoi

ggplot(data = dados, aes(x=dados$Vol_NaOH_pratico))+
  geom_boxplot(col ='blue',
               fill = 'lightblue')+
  coord_flip()



ts = replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)
range(ts)
pts = seq(-3.745,6.975,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')
lines(density(ts))
qqplot(ts,rt(1000,df=18))
abline(0,1)