library(tidyverse)
df <- read_tsv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/meta_dataset.txt")
df
cpiq <- subset(df, subset=(Design=="Control Prior IQ"))
poli <- subset(df, subset=(Design=="Policy Change"))

ggplot(data=poli) 
ggplot(aes(x=Outcome_age, y=Effect_size), data=poli) 
ggplot(aes(x=Outcome_age, y=Effect_size), data=poli) +
  geom_point() 

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point() 

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55) 

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825")

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_smooth()

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_smooth(method="lm")

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_smooth(method="lm", colour="#BA1825")

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825")

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0) + 
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + ggtitle("Policy Change")

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + ggtitle("Policy Change")+ 
  theme(plot.title = element_text(hjust=0.5))

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=df) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + ggtitle("Policy Change")+ 
  theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap(~Design)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=df %>% filter(Design != "School Age Cutoff")) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + ggtitle("Policy Change")+ 
  theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap(~Design)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=df %>% filter(Design != "School Age Cutoff")) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + ggtitle("Policy Change")+ 
  theme(plot.title = element_text(hjust=0.5)) +
  facet_grid(Design~.)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=df %>% filter(Design != "School Age Cutoff")) +
  geom_point(alpha=.55, colour="#BA1825") +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=F) +
  geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + 
  ggtitle("Effect of education as a function of age at the outcome test")+ 
  theme(plot.title = element_text(hjust=0.5)) +
  facet_grid(Design~.)

ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2), colour = Design, fill = Design), data=df %>% filter(Design != "School Age Cutoff")) +
  geom_point(alpha=.55) +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=FALSE, colour = FALSE, fill = FALSE) +
  geom_smooth(method="lm", size=.5, alpha=.25) + 
  ggtitle("Effect of education as a function of age at the outcome test")+ 
  theme(plot.title = element_text(hjust=0.5)) +
  facet_grid(Design~.)

meta_2_gg <- ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2), colour = Design, fill = Design), data=df %>% filter(Design != "School Age Cutoff")) +
  geom_point(alpha=.55) +
  geom_hline(yintercept=0, linetype="dotted") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
  xlab("Age at outcome test (years)") +
  ylab("Gain for 1 year of education\n(IQ points)") +
  guides(size=FALSE, colour = FALSE, fill = FALSE) +
  geom_smooth(method="lm", size=.5, alpha=.25) + 
  ggtitle("Effect of education as a function of age at the outcome test")+ 
  theme(plot.title = element_text(hjust=0.5)) +
  facet_grid(Design~.)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")
meta_2_gg



# 14.5 Расширения ggplot2 -------------------------------------------------

install.packages("hrbrthemes")
library(hrbrthemes)
meta_2_gg +
  theme_ipsum()