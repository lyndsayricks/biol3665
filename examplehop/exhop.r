# Cut down to middle 60 seconds
raw.data <- read.csv("exhop.csv")
library(dplyr)
raw.data <- raw.data %>% dplyr::filter(time > 30 & time < 90)

library(gsignal)
data <- raw.data
data$x[data$x < 0] = 0
peaks <- findpeaks((data$x), MinPeakDistance = 6, MinPeakHeight = 0.25)

# Just plot to help visualize
library(ggplot2)
library(ggpubr)
ggtheme <- theme(
  line=element_line(colour='#ad000e', size=0.8),
  plot.background=element_rect(fill='#ffffff'),
  panel.border=element_rect(colour='#ffffff', fill=NA, linewidth=0.6),
  panel.background=element_rect(colour='#ffffff', fill='#ffffff'),
  panel.grid.major.y=element_line(linewidth=0),
  panel.grid.major.x=element_line(linewidth=0),
  panel.grid.minor.x=element_line(linewidth=0),
  panel.grid.minor.y=element_line(linewidth=0),
  axis.line=element_line(linewidth=1, colour='#000000'),
  axis.ticks= element_blank(),
  axis.text=element_text(family='DejaVu Math TeX Gyre', colour='#000000', size=16),
  axis.title=element_text(family='DejaVu Math TeX Gyre', colour='#000000', size=18),
  plot.title=element_text(family='DejaVu Math TeX Gyre', colour='#000000', size=20, hjust = 0.5),
  axis.title.y=element_text(margin=unit(c(0.6,0.6,0.6,0.6), 'cm')),
  axis.title.x=element_text(margin=unit(c(0.6,0.6,0.6,0.6), 'cm')),
  plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm'),
  legend.text=element_text(family='DejaVu Math TeX Gyre'),
  legend.position='none'
)
plot1 <- ggplot() +
  geom_line(mapping=aes(x=raw.data$time, y=raw.data$x)) +
  scale_y_continuous(limits=c(-4, 4)) +
  labs(x='Time (s)', y='Ratio x acceleration:g') +
  ggtheme
plot2 <- ggplot() +
  geom_line(mapping=aes(x=data$time, y=data$x)) +
  geom_point(mapping=aes(x=data$time[peaks$loc], y=peaks$pks, color="red")) +
  scale_y_continuous(limits=c(-4,4)) +
  labs(x='Time (s)', y='Ratio x acceleration:g') +
  ggtheme

plots <- ggarrange(plot1, plot2, labels=c('a', 'b'), ncol=2, nrow=1, font.label=list(size=24, family='DejaVu Math TeX Gyre'))
png('../plots/examplehop.png', width=1024, height=512)
print(plots)
dev.off()