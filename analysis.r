# initialize dependencies
library(FSinR)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
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
  axis.ticks=element_blank(),
  axis.text=element_text(family='DejaVu Math TeX Gyre', colour='#000000', size=16),
  axis.title=element_text(family='DejaVu Math TeX Gyre', colour='#000000', size=18),
  plot.title=element_text(family='DejaVu Math TeX Gyre', colour='#000000', size=20, hjust = 0.5),
  axis.title.y=element_text(margin=unit(c(0.6,0.6,0.6,0.6), 'cm')),
  axis.title.x=element_text(margin=unit(c(0.6,0.6,0.6,0.6), 'cm')),
  plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm'),
  legend.text=element_text(family='DejaVu Math TeX Gyre', size=16),
  legend.title=element_blank()
)

# set up data
data <- read.csv('data.in.csv')
data$sex <- factor(data$sex)
data$subj.id <- c(1:8)
mean(abs(data$manual.freq-data$auto.freq))
freqs <- melt(data, id.vars='subj.id', measure.vars=c('auto.freq', 'manual.freq'), value.name='frequency', variable.name='freq.type')
freqs <- freqs %>% left_join(data.frame(subj.id = data$subj.id, freq.sd = data$freq.sd, height = data$height), by = 'subj.id')
freqs$freq.sd[freqs$freq.type == 'manual.freq'] = NA

# summary statistics
summary <- data %>% summarize(across(where(is.numeric), .fns = 
                          list(min = min,
                               median = median,
                               mean = mean,
                               stdev = sd,
                               q25 = ~quantile(., 0.25),
                               q75 = ~quantile(., 0.75),
                               max = max))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('variable', '.value'))

# plot the distribution of hopping frequencies
plot.preference <- ggplot(freqs, aes(x=subj.id, y=frequency, fill=freq.type)) +
  geom_col(position='dodge') +
  geom_errorbar(aes(ymin=frequency-freq.sd, ymax=frequency+freq.sd), width=0.2, position=position_dodge(.9)) +
  scale_y_continuous(limits=c(-0.05, 2)) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8)) +
  scale_fill_discrete(labels=c('Frequency I', 'Frequency II')) +
  labs(x='Subject', y='Preferred hopping frequency (Hz)') +
  ggtheme
png('plots/preference.png', width=768, height=512)
print(plot.preference)
dev.off()

# feature selection
relief_evaluator <- relief()
relief_evaluator(data,'auto.freq',c('age', 'sex', 'weight', 'height', 'rhr', 'hr.moderate', 'hr.vigorous', 'pfa.questionnaire'))
relief_evaluator(data,'manual.freq',c('age', 'sex', 'weight', 'height', 'rhr', 'hr.moderate', 'hr.vigorous', 'pfa.questionnaire'))

# regression analysis
summary(lm(auto.freq ~ height + weight + sex, data=data))
summary(lm(manual.freq ~ height + weight + sex, data=data))

# weight correlation with height?
summary(lm(weight ~ height, data=data))

# to confirm, no interaction effect
summary(lm(auto.freq ~ height + weight + height:weight + sex, data=data))
summary(lm(manual.freq ~ height + weight + height:weight + sex, data=data))

# height only
summary(lm(auto.freq ~ height, data=data))
summary(lm(manual.freq ~ height, data=data))

plot.lms <- ggplot(freqs, aes(x=height, y=frequency, color=factor(freq.type))) +
  geom_point(size=2, shape=19) +
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE) +
  scale_y_continuous(limits=c(0, 2)) +
  scale_x_continuous(limits=c(1.6, 1.9)) +
  scale_color_discrete(labels=c('Frequency I', 'Frequency II')) +
  labs(x='Height (m)', y='Preferred hopping frequency (Hz)') +
  ggtheme
png('plots/height.png', width=768, height=512)
print(plot.lms)
dev.off()