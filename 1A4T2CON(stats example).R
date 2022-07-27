library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(effectsize)


setwd("~/Desktop/pigeon Context-dependent 2019")
d <- read_xlsx("2019_pigeon_12conA4T.xlsx", sheet = "R")  
str(d)

d <- mutate(d,develope = factor(develope),nestID = factor(nestID),seq = factor(seq), treat = factor(treat),form = factor(form), inter=factor(inter))
d$T_ng <- as.numeric(d$T_ng)
d$A4_ng <- as.numeric(d$A4_ng)

d$treat <- factor(d$treat, levels = c("Con", "A4T"))
dfree <- d[which(d$form == "free"), ]
summary(dfree)

dfree$develope <- factor(dfree$develope, levels = c("undeveloped", "developed"))
dfree$inter <- factor(dfree$inter, levels = c("1st_Con", "1st_A4T","2nd_Con","2nd_A4T"))

hist(dfree$T_ng)
hist(dfree$A4_ng)

lmm_T <- lme4::glmer(T_ng ~ treat + seq + develope +
                     treat*develope + seq*develope +
                     (1|nestID), data = dfree, family = Gamma)

Anova(lmm_T)
summary(lmm_T)
rsquared(lmm_T)
r2_nakagawa(lmm_T, by_group = F, tolerance = 1e-5)

#to plot mean +-SD. whiskers are min-max,hinge of the box is SD
min.mean.sd.max <- function(x) {
        r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
}
ggplot(dfree, aes(develope, T_ng,color=inter)) +
        ylim(0,15)+
        scale_colour_brewer(palette="Dark2")+
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point( aes(color = inter),position = position_dodge(0.8))+
        theme(
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                strip.text = element_text(size=16)
        )



#nomality tests
shapiro.test(dfree$T_ng)#from the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(dfree$A4_ng)
#all checked, if not normal, use U-test


dfreeun <- dfree[which(dfree$develope == "undeveloped"), ]
dconjun <- dconj[which(dconj$develope == "undeveloped"), ]


dfreeun%>%
        group_by(inter)%>%
        summarise(mean = mean(T_ng, na.rm =T),
                  sd = sd(T_ng, na.rm =T)
        )



wilcox.test(dfreeun$T_ng[which(dfreeun$inter == "1st_Con")], 
       dfreeun$T_ng[which(dfreeun$inter == "1st_A4T")])
wilcox.test(dfreeun$T_ng[which(dfreeun$inter == "1st_Con")], 
       dfreeun$T_ng[which(dfreeun$inter == "2nd_Con")])
wilcox.test(dfreeun$T_ng[which(dfreeun$inter == "1st_A4T")], 
       dfreeun$T_ng[which(dfreeun$inter == "2nd_Con")])
wilcox.test(dfreeun$T_ng[which(dfreeun$inter == "2nd_Con")], 
       dfreeun$T_ng[which(dfreeun$inter == "2nd_A4T")])



#======================
#test for 1A4T vs 2Con
#======================
dfree1a2c <- dfree[which(dfree$inter == "1st_A4T"|dfree$inter == "2nd_Con"),]
dconj1a2c <- dconj[which(dconj$inter == "1st_A4T"|dconj$inter == "2nd_Con"),]


lmT <- lme4::glmer(T_ng ~ inter + develope +
                             inter*develope +
                             (1|nestID), data = dfree1a2c, family = Gamma)
qqnorm(resid(lmT))
qqline(resid(lmT))

Anova(lmT)
summary(lmT)






mT<- lmerTest::lmer(T_nmol/ave_allAndrogen_nmol ~ seq*treat+
                     (1|nestID), data = dper)
qqnorm(resid(mT))
qqline(resid(mT))
Anova(mT)
summary(mT)

mA4<- lmerTest::lmer(A4_nmol/ave_allAndrogen_nmol ~ seq*treat+
                            (1|nestID), data = dper)

Anova(mA4)
summary(mA4)

mconT<- lmerTest::lmer(conj_T_nmol/ave_allAndrogen_nmol ~ seq*treat+
                            (1|nestID), data = dper)

Anova(mconT)
summary(mconT)

