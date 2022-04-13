#Importing Library
pacman::p_load(tidyverse, dplyr, ggplot2, tidyr, AICcmodavg, knitr, readr)

#Importing datasets
# consumer_behaviour <- read.csv("C:/Users/rosha/Downloads/stat_data_file.csv")
consumer_behaviour <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Consumer_Behaviour/main/stat_data_file.csv")
head(consumer_behaviour)             #Looking into the data

#Looking for missing values
na_count <- colSums(is.na(consumer_behaviour))
na_count

#Create average columns for security, web design, reputation and price & deals variables
consumer_behaviour1 <- consumer_behaviour %>%
  mutate(sec_avg = as.integer((SEC_1 + SEC_2 + SEC_3 + SEC_4) / 4),
         web_avg = as.integer((WEB_1 + WEB_2 + WEB_3 + WEB_4) / 4),
         rep_avg = as.integer((REP_1 + REP_2 + REP_3 + REP_4 + REP_5) / 5),
         prc_avg = as.integer((PRC_1 + PRC_2 + PRC_3 + PRC_4 + PRC_5) / 5),
         att_avg = as.integer((ATT_1 + ATT_2 + ATT_3 + ATT_4) / 4))
head(consumer_behaviour1)

#Descriptive statistics
consumer_behaviour2 <- consumer_behaviour1 %>%
select(Age, Gender, Education, Income, sec_avg, web_avg, rep_avg, prc_avg, att_avg)
head(consumer_behaviour2)
summary(consumer_behaviour2)

#Plotting demographic Gender and Age
library("ggplot2")
ggplot(consumer_behaviour, aes(Age, fill=Gender)) +
geom_bar(position="dodge", width=0.85, colour="black") +
labs(title="Age of Respondents",
    x="Age in years",
    y="Frequency") +
theme_bw(base_size=14) +
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle=90),
       legend.text = element_text(size = 14),
       plot.title = element_text(size=16)) +
facet_wrap(~Gender)

#Plotting gender and educational qualilification of Respondents
ggplot(consumer_behaviour, aes(x=Education, fill=Gender)) +
geom_bar(width=0.6, colour="black", position="dodge") +
labs(title="Educational qualification based on gender",
    x="Gender",
    y="Frequency") +
coord_flip() +
theme_bw(base_size=14) +
theme(axis.text = element_text(size=14),
     legend.text=element_text(size=14),
     plot.title=element_text(size=16)) 

#Plotting Income distribution of the respondents
ggplot(consumer_behaviour, aes(x=Income, fill=Gender)) +
geom_bar(width=0.6, colour="black", position="dodge") +
labs(title="Income based on gender and educational qualification",
    x="Gender",
    y="Frequency") +
# coord_flip() +
theme_bw(base_size=14) +
theme(axis.text = element_text(size=14),
      axis.text.x = element_text(angle=90),
     legend.text=element_text(size=14),
     plot.title=element_text(size=16)) +
facet_wrap(~Education)

#Plotting security variable
ggplot(consumer_behaviour1, aes(Gender, sec_avg, color=Gender)) +
geom_violin(trim=FALSE, size=1.5) +
geom_point() +
geom_jitter(position=position_jitter(0.2)) +
stat_summary(fun=median, geom="point", shape=16,
                 size=3, color="Black") +
facet_wrap(~Income) +
scale_y_continuous(name="Scale", breaks=c(1,2,3,4,5)) +
labs(title="Security", x="Gender", y="") +
theme_bw(base_size=13) +
theme(axis.text=element_text(size=13),
     legend.text=element_text(size=13),
     legend.position="bottom",
     plot.title=element_text(size=16, face="bold"))

#Plotting Web design variable
ggplot(consumer_behaviour1, aes(Gender, web_avg, color=Gender)) +
geom_violin(trim=FALSE, size=1.5) +
geom_jitter(height=0.1) +
stat_summary(fun=median, geom="point", shape=16, size=3, color="black") +
facet_wrap(~Income) +
scale_y_continuous(name="Scale", breaks=c(1,2,3,4,5)) +
labs(title="Web Design Scale", x="Gender", y="") +
theme_bw(base_size=13) +
theme(axis.text=element_text(size=13),
     legend.text=element_text(size=13),
     legend.position="bottom",
     plot.title=element_text(size=16, face="bold"))

#Plotting Reputation scale
ggplot(consumer_behaviour1, aes(Gender, rep_avg, color=Gender)) +
geom_violin(trim=FALSE, size=1.5) +
geom_jitter(height=0.1) +
stat_summary(fun=median, geom="point", shape=16,
            color="black", size=3) +
facet_wrap(~Income) +
scale_y_continuous(name="Scale", breaks=c(1,2,3,4,5)) +
labs(title="Reputation Scale", x="Gender", y="") +
theme_bw(base_size=13) +
theme(axis.text=element_text(size=13),
     legend.text=element_text(size=13),
     legend.position="bottom",
     plot.title=element_text(size=16, face="bold"))

#Plotting price and deals scale
ggplot(consumer_behaviour1, aes(Gender, prc_avg, color=Gender)) +
geom_violin(trim=FALSE, size=1.5) +
geom_jitter(height=0.1) +
stat_summary(fun=median, geom="point", shape=16,
            color="black", size=3) +
#geom_boxplot(width=0.1) +
facet_wrap(~Income) +
scale_y_continuous(name="Scale", breaks=c(1,2,3,4,5)) +
labs(title="Price & Deals Scale", x="Gender", y="") +
theme_bw(base_size=13) +
theme(axis.text=element_text(size=13),
     legend.text=element_text(size=13),
     legend.position="bottom",
     plot.title=element_text(size=16, face="bold"))

#Plotting Attitude scale
ggplot(consumer_behaviour1, aes(Gender, att_avg, color=Gender)) +
geom_violin(trim=FALSE, size=1.5) +
geom_jitter(height=0.1) +
stat_summary(fun=median, geom="point", shape=16,
            color="black", size=3) +
#geom_boxplot(width=0.1) +
facet_wrap(~Income) +
scale_y_continuous(name="Scale", breaks=c(1,2,3,4,5)) +
labs(title="Attitude Scale", x="Gender", y="") +
theme_bw(base_size=13) +
theme(axis.text=element_text(size=13),
     legend.text=element_text(size=13),
     legend.position="bottom",
     plot.title=element_text(size=16, face="bold"))

#Forming the regression models
sec_avg.mod <- lm(att_avg ~ sec_avg, data = consumer_behaviour1)
web_avg.mod <- lm(att_avg ~ web_avg, data = consumer_behaviour1)
rep_avg.mod <- lm(att_avg ~ rep_avg, data = consumer_behaviour1)
prc_avg.mod <- lm(att_avg ~ prc_avg, data = consumer_behaviour1)
sec_avg.web_avg.mod <- lm(att_avg ~ sec_avg + web_avg, data = consumer_behaviour1)
sec_avg.rep_avg.mod <- lm(att_avg ~ sec_avg + rep_avg, data = consumer_behaviour1)
web_avg.rep_avg.mod <- lm(att_avg ~ web_avg + rep_avg, data = consumer_behaviour1)
sec_avg.web_avg.rep_avg.mod <- lm(att_avg ~ sec_avg + web_avg + rep_avg, data = consumer_behaviour1)
web_avg.rep_avg.prc_avg.mod <- lm(att_avg ~ web_avg + rep_avg + prc_avg, data = consumer_behaviour1)
rep_avg.prc_avg.sec_avg.mod <- lm(att_avg ~ sec_avg + rep_avg + prc_avg, data = consumer_behaviour1)
combination.mod <- lm(att_avg ~ sec_avg + web_avg + rep_avg + prc_avg, data = consumer_behaviour1)

#Comparing the models
library("AICcmodavg")
models <- list(sec_avg.mod, web_avg.mod, rep_avg.mod, prc_avg.mod, sec_avg.web_avg.mod, sec_avg.rep_avg.mod, web_avg.rep_avg.mod, sec_avg.web_avg.rep_avg.mod, web_avg.rep_avg.prc_avg.mod, rep_avg.prc_avg.sec_avg.mod, combination.mod)
models.name <- c("sec_avg.mod", "web_avg.mod", "rep_avg.mod", "prc_avg.mod", "sec_avg.web_avg.mod", "sec_avg.rep_avg.mod", "web_avg.rep_avg.mod", "sec_avg.web_avg.rep_avg.mod",  "web_avg.rep_avg.prc_avg.mod", "rep_avg.prc_avg.sec_avg.mod", "combination.mod")
aictab(cand.set = models, modnames = models.name) %>%
knitr::kable()

#Plotting residuals
m1 <- lm(att_avg ~ sec_avg + web_avg + rep_avg, data = consumer_behaviour1)
res <- resid(sec_avg.web_avg.rep_avg.mod)
plot(fitted(sec_avg.web_avg.rep_avg.mod), res)
abline(0,0)

#Plotting Q-Q plot
qqnorm(res)
qqline(res)

#Regression Analysis
model <- lm(att_avg ~ sec_avg + web_avg  + rep_avg, data = consumer_behaviour1)
summary(model)

#Anova
anova <- aov(att_avg ~ sec_avg + web_avg + rep_avg, data = consumer_behaviour1)
summary(anova)
