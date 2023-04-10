# initialize dependencies
library(FSinR)
library(ggplot2)
library(data.table)
library(car)
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
  legend.title=element_blank()
)

# set up data
data <- read.csv('data.in.csv')
data$sex <- factor(data$sex)
data$subj.id <- c(1:8)
freqs <- melt(data, id.vars='subj.id', measure.vars=c('auto.freq', 'manual.freq'), value.name='frequency', variable.name='freq.type')
freqs <- freqs %>% left_join(data.frame(subj.id = data$subj.id, freq.sd = data$freq.sd), by = 'subj.id')
freqs$freq.sd[freqs$freq.type == 'manual.freq'] = NA

# plot the distribution of hopping frequencies
plot.preference <- ggplot(freqs, aes(x=subj.id, y=frequency, fill=freq.type)) +
  geom_col(position='dodge') +
  geom_errorbar(aes(ymin=frequency-freq.sd, ymax=frequency+freq.sd), width=0.2, position=position_dodge(.9)) +
  scale_y_continuous(limits=c(-0.05, 2)) +
  scale_x_continuous(breaks=c(1:8), labels=c(1:8)) +
  scale_fill_discrete(labels=c("Average computed frequency", "Manual frequency count")) +
  labs(x='Subject', y='Preferred hopping frequency') +
  ggtheme
png('plots/preference.png', width=768, height=512)
print(plot.preference)
dev.off()

# feature selection
relief_evaluator <- relief()
relief_evaluator(data,'auto.freq',c('age', 'sex', 'weight', 'height', 'rhr', 'hr.moderate', 'hr.vigorous', 'pfa.questionnaire'))
relief_evaluator(data,'manual.freq',c('age', 'sex', 'weight', 'height', 'rhr', 'hr.moderate', 'hr.vigorous', 'pfa.questionnaire'))

# analyses of variation
data.contrasts <- list(height = contr.sum, sex = contr.sum, weight = contr.sum)
model.a <- lm(auto.freq ~ height + weight + sex, data=data)
anova.a <- Anova(model.a, contrasts=data.contrasts, type=2, singular.ok=TRUE)
model.m <- lm(manual.freq ~ height + weight + sex, data=data)
anova.m <- Anova(model.m, contrasts=data.contrasts, type=2, singular.ok=TRUE)