library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(effectsize)
library(piecewiseSEM)
library(performance)

setwd("~/Desktop/pigeon Context-dependent 2019")
d <- read_xlsx("2019_pigeon_12conA4T.xlsx", sheet = "R")  
str(d)

d <- mutate(d,develope = factor(develope),nestID = factor(nestID),seq = factor(seq), treat = factor(treat),form = factor(form), inter=factor(inter))
d$T_ng <- as.numeric(d$T_ng)
d$A4_ng <- as.numeric(d$A4_ng)
d$etio_ng <- as.numeric(d$etio_ng)

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

lmm_A4 <- glmer(A4_ng ~ treat + seq + develope +
                     treat*develope + seq*develope +
                     (1|nestID), data = dfree, family = Gamma(link = "inverse"))# inverse is the default link, try "log" link and check for qqplot

#qqnorm(resid(lmm_A4))
#qqline(resid(lmm_A4))

Anova(lmm_A4)
summary(lmm_A4)
rsq(lm_A4,type=c('v','kl','sse','lr','n') )
r2_nakagawa(lm_A4, by_group = F, tolerance = 1e-5)



lmm_E <- glmer(etio_ng ~ treat + seq + develope +
                      treat*develope + seq*develope +
                      (1|nestID), data = dfree,family = Gamma(link = "inverse"))


summary(lmm_E)
Anova(lmm_E)
rsq(lm_E,type=c('v','kl','sse','lr','n') )
rsquared(lm_E)
r2_nakagawa(lm_E, by_group = T, tolerance = 1e-5)



 dconj <- d[which(d$form == "conjugated"), ]
 dconj$develope <- factor(dconj$develope, levels = c("undeveloped", "developed"))
 dconj$inter <- factor(dconj$inter, levels = c("1st_Con", "1st_A4T","2nd_Con","2nd_A4T"))
summary(dconj)

lmm_T_c <- glmer(T_ng ~ treat + seq + develope +
                     treat*develope + seq*develope +
                     (1|nestID), data = dconj,family = Gamma)

lm_T_c <- glm(T_ng ~ treat + seq + develope +
                         treat*develope + seq*develope, data = dconj,family = Gamma)
summary(lmm_T_c)
Anova(lmm_T_c)
r2_nakagawa(lm_T_c, by_group = F, tolerance = 1e-5)
rsq(lm_T_c,type=c('v','kl','sse','lr','n') )


lmm_E_c <- glmer(etio_ng ~ treat + seq + develope +
                     treat*develope + seq*develope +
                     (1|nestID), data = dconj,family = Gamma(link = "log"))

lm_E_c <- erlm(log10(etio_ng) ~ treat + seq + develope +
                         treat*develope + seq*develope+
                       (1|nestID) , data = dconj)
#qqnorm(resid(lm_E_c))
#qqline(resid(lm_E_c))

summary(lmm_E_c)
Anova(lmm_E_c)
r2_nakagawa(lm_E_c, by_group = F, tolerance = 1e-5)
rsq(lm_E_c,type=c('v','kl','sse','lr','n') )


#nomality tests
shapiro.test(dfree$T_ng)#from the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(dfree$A4_ng)
shapiro.test(dfree$etio_ng)
shapiro.test(dconj$T_ng)
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

wilcox.test(dfreeun$A4_ng[which(dfreeun$inter == "1st_Con")], 
       dfreeun$A4_ng[which(dfreeun$inter == "1st_A4T")])
wilcox.test(dfreeun$A4_ng[which(dfreeun$inter == "1st_Con")], 
       dfreeun$A4_ng[which(dfreeun$inter == "2nd_Con")])
wilcox.test(dfreeun$A4_ng[which(dfreeun$inter == "1st_A4T")], 
       dfreeun$A4_ng[which(dfreeun$inter == "2nd_Con")])
wilcox.test(dfreeun$A4_ng[which(dfreeun$inter == "2nd_Con")], 
       dfreeun$A4_ng[which(dfreeun$inter == "2nd_A4T")])

wilcox.test(dconjun$T_ng[which(dconjun$inter == "1st_Con")], 
       dconjun$T_ng[which(dconjun$inter == "1st_A4T")])
wilcox.test(dconjun$T_ng[which(dconjun$inter == "1st_Con")], 
       dconjun$T_ng[which(dconjun$inter == "2nd_Con")])
wilcox.test(dconjun$T_ng[which(dconjun$inter == "1st_A4T")], 
       dconjun$T_ng[which(dconjun$inter == "2nd_Con")])
wilcox.test(dconjun$T_ng[which(dconjun$inter == "2nd_Con")], 
       dconjun$T_ng[which(dconjun$inter == "2nd_A4T")])

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



lmA4 <- lme4::glmer(A4_ng ~ inter + develope +
                           inter*develope +
                           (1|nestID), data = dfree1a2c, family = Gamma)
Anova(lmA4)
summary(lmA4)



lmE <- lme4::glmer(etio_ng ~ inter + develope +
                            inter*develope +
                            (1|nestID), data = dfree1a2c, family = Gamma(link = "log"))
Anova(lmE)
summary(lmE)



lmTconj <- lme4::glmer(T_ng ~ inter + develope +
                           inter*develope +
                           (1|nestID), data = dconj1a2c, family = Gamma)
qqnorm(resid(lmTconj))
qqline(resid(lmTconj))

Anova(lmTconj)
summary(lmTconj)



lmEconj <- lme4::glmer(etio_ng ~ inter + develope +
                               inter*develope +
                               (1|nestID), data = dconj1a2c, family = Gamma(link = "log"))

qqnorm(resid(lmEconj))
qqline(resid(lmEconj))

Anova(lmEconj)
summary(lmEconj)



hist((dper$T_nmol/dper$ave_allAndrogen_nmol))
hist((dper$A4_nmol/dper$ave_allAndrogen_nmol))






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

mE<- lmerTest::lmer(etio_nmol/ave_allAndrogen_nmol ~ seq*treat+
                               (1|nestID), data = dper)

Anova(mE)
summary(mE)


mconE<- lmerTest::lmer(conj_etio_nmol/ave_allAndrogen_nmol ~ seq*treat+
                            (1|nestID), data = dper)

Anova(mconE)
summary(mconE)


mconvertA4<- lmerTest::lmer(((A4_nmol-ave_A4_nmol)/ave_allAndrogen_nmol) ~ seq*treat    
                           +(1|nestID), data = dper)
summary(mconvertA4)


mconvertT<- lmerTest::lmer(((T_nmol-ave_T_nmol)/ave_allAndrogen_nmol) ~ seq*treat    
                           +(1|nestID), data = dper)
summary(mconvertT)

mconvertconT<- lmerTest::lmer(((conj_T_nmol-ave_conj_T_nmol)/ave_allAndrogen_nmol) ~ seq*treat    
                           +(1|nestID), data = dper)
summary(mconvertconT)

mconvertA<- lmerTest::lmer(((ave_T_nmol+ave_A4_nmol-A4_nmol-T_nmol)/ave_allAndrogen_nmol) ~ seq*treat    
                              +(1|nestID), data = dper)
summary(mconvertA)



setwd("~/Desktop/pigeon Context-dependent 2019")
d <- read_xlsx("2019_pigeon_12conA4T.xlsx", sheet = "Sheet1")  
str(d)

library(irr)
kendall(d, TRUE)
