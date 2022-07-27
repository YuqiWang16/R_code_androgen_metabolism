library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)

setwd("~/Desktop/pigeon Context-dependent 2019")

dapi <- read_excel("2019_pigeon_12conA4T.xlsx", sheet = "plot")
str(dapi)
dapi <- mutate(dapi,nestID = factor(nestID),
               treat = factor(treat), 
               seq = factor(seq), 
               develope = factor(develope),
               inter = factor(inter))

dapi$etio_ng <- as.numeric(dapi$etio_ng)
dapi$T_ng<- as.numeric(dapi$T_ng)
dapi$A4_ng<- as.numeric(dapi$A4_ng)
dapi$conj_etio_ng <- as.numeric(dapi$conj_etio_ng)
dapi$conj_T_ng<- as.numeric(dapi$conj_T_ng)

str(dapi)
summary(dapi)

dapi$develope <- factor(dapi$develope, levels = c("undeveloped", "developed"))
dapi$inter <- factor(dapi$inter, levels = c("1st_Con", "1st_A4T","2nd_Con","2nd_A4T"))


#to plot mean +-SD. whiskers are min-max,hinge of the box is SD
min.mean.sd.max <- function(x) {
        r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
}

scaleFUN <- function(x) sprintf("%.0f", x) # for one dec on y-axis

pdapi1 <- ggplot(dapi, aes(x=develope, y=etio_ng,  shape = inter)) +
        #scale_fill_manual(values=c("#97BC62FF","#36AD1F"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Etiocholanolone (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width = 0.75)+
        stat_summary(fun.y=mean, geom="line", aes(group = inter,linetype = seq),lwd=0.75,position = position_dodge(0.8))+
        scale_linetype_manual(values=c("solid","dashed"))+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        theme_classic()+
        theme(legend.position = "none")+
        font("ylab", size = 18)+
        font("xy.text", size = 18)
        # add for the skew lines
#+theme(legend.text = element_text(size = 14))


pdapi3 <- ggplot(dapi, aes(x=develope, y=T_ng,shape = inter)) +
        #scale_fill_manual(values=c("#9CC3D5FF","#00B1D2FF"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Testosterone (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width = 0.75)+
        stat_summary(fun.y=mean, geom="line", aes(group = inter,linetype = seq),lwd=0.75,position = position_dodge(0.8))+
        scale_linetype_manual(values=c("solid","dashed"))+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        theme_classic()+
        theme(legend.position = "none")+
        font("ylab", size = 18)+
        font("xy.text", size = 18)
        

pdapi4 <- ggplot(dapi, aes(x=develope, y=A4_ng, shape = inter)) +
        #cale_fill_manual(values=c("#FFEE99","#FFD500"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Androstenedione (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width = 0.75)+
        stat_summary(fun.y=mean, geom="line", aes(group = inter,linetype = seq),lwd=0.75,position = position_dodge(0.8))+
        scale_linetype_manual(values=c("solid","dashed"))+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        theme_classic()+
        theme(legend.position = "none")+
        font("ylab", size = 18)+
        font("xy.text", size = 18)
        


pdapi7 <- ggplot(dapi, aes(x=develope, y=conj_etio_ng, shape = inter)) +
        #scale_fill_manual(values=c("#E5E5E5","#999999"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Conjuagted etiocholanolone (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width = 0.75)+
        stat_summary(fun.y=mean, geom="line", aes(group = inter,linetype = seq),lwd=0.75,position = position_dodge(0.8))+
        scale_linetype_manual(values=c("solid","dashed"))+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        theme_classic()+
        theme(legend.position = "none")+
        font("ylab", size = 18)+
        font("xy.text", size = 18)


pdapi6 <- ggplot(dapi, aes(x=develope, y=conj_T_ng, shape = inter)) +
        #scale_fill_manual(values=c("#E5E5E5","#999999"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Conjugated testosterone (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width = 0.75)+
        stat_summary(fun.y=mean, geom="line", aes(group = inter,linetype = seq),lwd=0.75,position = position_dodge(0.8))+
        scale_linetype_manual(values=c("solid","dashed"))+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        theme_classic()+
        theme(legend.position = "none")+
        font("ylab", size = 18)+
        font("xy.text", size = 18)
#+scale_y_continuous(labels=scaleFUN)# for one dec on y-axis


plot_grid(pdapi4,NULL, pdapi3, pdapi1, pdapi6,pdapi7,
          align = "hv", axis = "tb",labels = c("A", "","B","D","C","E"),
          ncol =2, nrow =3,greedy = T,
          hjust = c(-7.5, -7.5,-7.5,-7.5,-7.5,-8.5), vjust = 1)
#export with width = 1000, height =1200



#=======================================================================
#
#stats
#
###
library(lme4)
library(car)
library(sjPlot)

lm_T <- lmer(T_ng ~ treat + seq + develope +
                     treat*develope + seq*develope +
                     (1|nestID), data = dapi)
Anova(lm_T)
tab_model(lm_T, p.val = "kr", show.df = TRUE)

lm_A4 <- lmer(A4_ng ~ treat + seq + develope +
                      treat*develope + seq*develope +
                      (1|nestID), data = dapi)
Anova(lm_A4)
tab_model(lm_A4, p.val = "kr", show.df = TRUE)

lm_E <- lmer(etio_ng ~ treat + seq + develope +
                     treat*develope + seq*develope +
                     (1|nestID), data = dapi)
Anova(lm_E)
tab_model(lm_E, p.val = "kr", show.df = TRUE)


dconj <- dapi[which(dapi$form == "conjugated"), ]
summary(dconj)

lm_T_c <- lmer(T_ng ~ treat + seq + develope +
                       treat*develope + seq*develope +
                       (1|nestID), data = dconj)
Anova(lm_T_c)
tab_model(lm_T_c, p.val = "kr", show.df = TRUE)

lm_E_c <- lmer(etio_ng ~ treat + seq + develope +
                       treat*develope + seq*develope +
                       (1|nestID), data = dconj)
Anova(lm_E_c)
tab_model(lm_E_c, p.val = "kr", show.df = TRUE)

#nomality tests
shapiro.test(dapi$T_ng)#from the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(dapi$A4_ng)
shapiro.test(dapi$etio_ng)
shapiro.test(dconj$T_ng)
#all checked, if not normal, use U-test


dapiun <- dapi[which(dapi$develope == "undeveloped"), ]
dconjun <- dconj[which(dconj$develope == "undeveloped"), ]

leveneTest(T_ng ~ treat*seq, data = dapiun)
leveneTest(A4_ng ~ treat*seq, data = dapiun)#first check for equal variation

wilcox.test(dapiun$T_ng[which(dapiun$inter == "1st_Con")], 
            dapiun$T_ng[which(dapiun$inter == "1st_A4T")])
wilcox.test(dapiun$T_ng[which(dapiun$inter == "1st_Con")], 
            dapiun$T_ng[which(dapiun$inter == "2nd_Con")])
wilcox.test(dapiun$T_ng[which(dapiun$inter == "1st_A4T")], 
            dapiun$T_ng[which(dapiun$inter == "2nd_Con")])
wilcox.test(dapiun$T_ng[which(dapiun$inter == "2nd_Con")], 
            dapiun$T_ng[which(dapiun$inter == "2nd_A4T")])

wilcox.test(dapiun$A4_ng[which(dapiun$inter == "1st_Con")], 
            dapiun$A4_ng[which(dapiun$inter == "1st_A4T")])
wilcox.test(dapiun$A4_ng[which(dapiun$inter == "1st_Con")], 
            dapiun$A4_ng[which(dapiun$inter == "2nd_Con")])
wilcox.test(dapiun$A4_ng[which(dapiun$inter == "1st_A4T")], 
            dapiun$A4_ng[which(dapiun$inter == "2nd_Con")])
wilcox.test(dapiun$A4_ng[which(dapiun$inter == "2nd_Con")], 
            dapiun$A4_ng[which(dapiun$inter == "2nd_A4T")])

t.test(dconjun$T_ng[which(dconjun$inter == "1st_Con")], 
       dconjun$T_ng[which(dconjun$inter == "1st_A4T")])
t.test(dconjun$T_ng[which(dconjun$inter == "1st_Con")], 
       dconjun$T_ng[which(dconjun$inter == "2nd_Con")])
t.test(dconjun$T_ng[which(dconjun$inter == "1st_A4T")], 
       dconjun$T_ng[which(dconjun$inter == "2nd_Con")])
t.test(dconjun$T_ng[which(dconjun$inter == "2nd_Con")], 
       dconjun$T_ng[which(dconjun$inter == "2nd_A4T")])







#compare only 1.A4T and 2.Con
dapi1A2C <- read_excel("pigeon_2019_1.2.A4T.Con.xlsx", sheet = "Sheet4")
str(dapi1A2C)
dapi1A2C <- mutate(dapi1A2C,egg_ID = factor(egg_ID),treat = factor(treat), seq = factor(seq), form = factor(form), develope = factor(develope), inter = factor(inter))

dapi1A2C$etio_ng_g <- as.numeric(dapi1A2C$etio_ng_g)
dapi1A2C$T_ng_g<- as.numeric(dapi1A2C$T_ng_g)
dapi1A2C$A4_ng_g<- as.numeric(dapi1A2C$A4_ng_g)

dapi1A2C <- dapi1A2C[which(dapi1A2C$form == "free"), ]
str(dapi1A2C)
summary(dapi1A2C)

dapi1A2C$develope <- factor(dapi1A2C$develope, levels = c("undeveloped", "developed"))

lm_A4 <- lm(A4_ng_g ~ inter*develope , data=dapi1A2C) 
Anova(lm_A4)# type II test from car

lm_T <- lm(T_ng_g ~ inter*develope , data=dapi1A2C) 
Anova(lm_T)# type II test from car

lm_etio <- lm(etio_ng_g ~ inter*develope , data=dapi1A2C) 
Anova(lm_etio)# type II test from car



#post-hoc
dapi_un <- dapi[which(dapi$develope == "undeveloped"), ]
dapi_f <- dapi[which(dapi$develope == "developed"), ]

lm_A4 <- lmer(A4_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_A4)# type II test from car
lm_A4 <- lm(A4_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_A4)

lm_prog <- lmer(prog_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_prog)# type II test from car
lm_prog <- lm(prog_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_prog)

lm_etio <- lmer(etio_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_etio)# type II test from car
lm_etio <- lm(etio_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_etio)

lm_preg <- lmer(preg_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_preg)# type II test from car
lm_preg <- lm(preg_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_preg)

lm_T <- lmer(T_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_T)# type II test from car
lm_T <- lm(T_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_T)

lm_OH_proges <- lmer(OH_proges_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_OH_proges)# type II test from car
lm_OH_proges <- lm(OH_proges_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_OH_proges)


dapi_1 <- dapi[which(dapi$seq == "1"), ]
dapi_2 <- dapi[which(dapi$seq == "2"), ]

lm_A4 <- lm(A4_ng_g ~ treat*develope, data=dapi_1) 
Anova(lm_A4)# type II test from car
lm_A4 <- lmer(A4_ng_g ~ treat*develope + (1|egg_ID), data=dapi_2) 
Anova(lm_A4)

lm_prog <- lm(prog_ng_g ~ treat*develope , data=dapi_1) 
Anova(lm_prog)# type II test from car
lm_prog <- lmer(prog_ng_g ~ treat*develope + (1|egg_ID), data=dapi_2) 
Anova(lm_prog)

lm_etio <- lm(etio_ng_g ~ treat*develope, data=dapi_1) 
Anova(lm_etio)# type II test from car
lm_etio <- lmer(etio_ng_g ~ treat*develope + (1|egg_ID), data=dapi_2) 
Anova(lm_etio)

lm_preg <- lm(preg_ng_g ~ treat*develope , data=dapi_1) 
Anova(lm_preg)# type II test from car
lm_preg <- lmer(preg_ng_g ~ treat*develope + (1|egg_ID), data=dapi_2) 
Anova(lm_preg)

lm_T <- lm(T_ng_g ~ treat*develope , data=dapi_1) 
Anova(lm_T)# type II test from car
lm_T <- lmer(T_ng_g ~ treat*develope + (1|egg_ID), data=dapi_2) 
Anova(lm_T)

lm_OH_proges <- lm(OH_proges_ng_g ~ treat*develope , data=dapi_1) 
Anova(lm_OH_proges)# type II test from car
lm_OH_proges <- lmer(OH_proges_ng_g ~ treat*develope + (1|egg_ID), data=dapi_2) 
Anova(lm_OH_proges)

###
# mutiple t-tests
# undeveloped


dapi_un %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "1st_A4T"])$p.value), vars = c(A4_ng, etio_ng, T_ng))
dapi_un %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "2nd_Con"])$p.value), vars = c(A4_ng, etio_ng, T_ng))
dapi_un %>%
        summarise_each(funs(t.test(.[inter == "1st_A4T"], .[inter == "2nd_Con"])$p.value), vars = c(A4_ng, etio_ng, T_ng))
dapi_un %>%
        summarise_each(funs(t.test(.[inter == "2nd_Con"], .[inter == "2nd_A4T"])$p.value), vars = c(A4_ng, etio_ng, T_ng))

# developed
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "1st_A4T"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "2nd_Con"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "2nd_A4T"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_A4T"], .[inter == "2nd_Con"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_A4T"], .[inter == "2nd_A4T"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "2nd_Con"], .[inter == "2nd_A4T"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))


# pairwise undeveloped vs. developed
dapi_1c <- dapi[which(dapi$inter == "1st_Con"), ]
dapi_1a <- dapi[which(dapi$inter == "1st_A4T"), ]
dapi_2c <- dapi[which(dapi$inter == "2nd_Con"), ]
dapi_2a <- dapi[which(dapi$inter == "2nd_A4T"), ]

dapi_1c %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_1a %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_2c %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))
dapi_2a %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(A4_ng_g, prog_ng_g, etio_ng_g, preg_ng_g, T_ng_g, OH_proges_ng_g))


# if difference different?
dapi_1a2c <- dapi[which(dapi$inter == "2nd_Con"|dapi$inter == "1st_A4T"), ]
str(dapi_1a2c)
lm_A4 <- lm(A4_ng_g ~ inter*develope, data=dapi_1a2c) 
Anova(lm_A4)# type II test from car

lm_prog <- lm(prog_ng_g ~ inter*develope, data=dapi_1a2c) 
Anova(lm_prog)# type II test from car

lm_etio <- lm(etio_ng_g ~ inter*develope, data=dapi_1a2c) 
Anova(lm_etio)# type II test from car

lm_preg <- lm(preg_ng_g ~ inter*develope, data=dapi_1a2c) 
Anova(lm_preg)# type II test from car

lm_T <- lm(T_ng_g ~ inter*develope, data=dapi_1a2c) 
Anova(lm_T)# type II test from car

lm_OH_proges <- lm(OH_proges_ng_g ~ inter*develope, data=dapi_1a2c) 
Anova(lm_OH_proges)# type II test from car












#===========================================
#
### conjugates
#
setwd("C:/Users/p285080/Desktop/2019_pigeon_12conA4T")
dapi <- read_excel("2019_pigeon_12conA4T.xlsx", sheet = "R")
str(dapi)
dapi <- mutate(dapi,nestID = factor(nestID),
               treat = factor(treat), 
               seq = factor(seq), 
               form = factor(form), 
               develope = factor(develope),
               inter = factor(inter))

dapi$etio_ng <- as.numeric(dapi$etio_ng)

dapi$T_ng<- as.numeric(dapi$T_ng)

dapi$A4_ng<- as.numeric(dapi$A4_ng)

dapi$develope <- factor(dapi$develope, levels = c("undeveloped", "developed"))
dapi$inter <- factor(dapi$inter, levels = c("1st_Con", "1st_A4T","2nd_Con","2nd_A4T"))

dconj <- dapi[which(dapi$form == "conjugated"), ]

str(dapi)
summary(dapi)

#to plot mean +-SD. whiskers are min-max,hinge of the box is SD
min.mean.sd.max <- function(x) {
        r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
}

pdapi7 <- ggplot(dconj, aes(x=develope, y=etio_ng, shape = inter,fill=develope )) +
        scale_fill_manual(values=c("#E5E5E5","#999999"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Conjuagted etiocholanolone (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width = 0.75)+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1.5)+
        theme_classic()+
        font("ylab", size = 18)+
        font("xy.text", size = 18)
        #+scale_y_continuous(labels=scaleFUN)# for one dec on y-axis


pdapi6 <- ggplot(dconj, aes(x=develope, y=T_ng, shape = inter,fill=develope, )) +
        scale_fill_manual(values=c("#E5E5E5","#999999"))+
        scale_shape_manual(values=c(1,19,0,15)) +
        ylab("Conjugated testosterone (ng/egg)") + xlab("")+
        stat_summary(fun.data = min.mean.sd.max, geom = "boxplot",position = position_dodge(0.8),lwd=0.75,width=0.75)+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1.5)+
        theme_classic()+
        font("ylab", size = 18)+
        font("xy.text", size = 18)
         #+scale_y_continuous(labels=scaleFUN)# for one dec on y-axis


plot_grid(pdapi1, pdapi3, align = "hv", axis = "tb",labels="AUTO",ncol =2, nrow =3)


lm_etio <- lmer(etio_ng ~ treat+seq+develope+develope*treat + develope*seq+ (1|nestID), data=dapi) 
Anova(lm_etio)# type II test from car


lm_T <- lmer(T_ng ~ treat+seq+develope+develope*treat + develope*seq+ (1|nestID), data=dapi) 
Anova(lm_T)# type II test from car


#post-hoc
dapi_un <- dapi[which(dapi$develope == "undeveloped"), ]
dapi_f <- dapi[which(dapi$develope == "developed"), ]

lm_etio <- lmer(etio_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_etio)# type II test from car
lm_etio <- lm(etio_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_etio)

lm_T <- lmer(T_ng_g ~ treat*seq + (1|egg_ID), data=dapi_un) 
Anova(lm_T)# type II test from car
lm_T <- lm(T_ng_g ~ treat*seq, data=dapi_f) 
Anova(lm_T)


dapi_1 <- dapi[which(dapi$seq == "1"), ]
dapi_2 <- dapi[which(dapi$seq == "2"), ]

lm_etio <- lm(etio_ng_g ~ treat*develope , data=dapi_1) 
Anova(lm_etio)# type II test from car
lm_etio <- lmer(etio_ng_g ~ treat*develope+ (1|egg_ID), data=dapi_2) 
Anova(lm_etio)

lm_T <- lm(T_ng_g ~ treat*develope , data=dapi_1) 
Anova(lm_T)# type II test from car
lm_T <- lmer(T_ng_g ~ treat*develope+ (1|egg_ID), data=dapi_2) 
Anova(lm_T)


# mutiple t-tests
# undeveloped
dapi_un %>%
        summarise_each(funs(wilcox.test(.[inter == "1st_Con"], .[inter == "1st_A4T"])$p.value), vars = c(etio_ng, T_ng))
dapi_un %>%
        summarise_each(funs(wilcox.test(.[inter == "1st_Con"], .[inter == "2nd_Con"])$p.value), vars = c(etio_ng, T_ng))
dapi_un %>%
        summarise_each(funs(wilcox.test(.[inter == "1st_A4T"], .[inter == "2nd_Con"])$p.value), vars = c(etio_ng, T_ng))
dapi_un %>%
        summarise_each(funs(wilcox.test(.[inter == "2nd_Con"], .[inter == "2nd_A4T"])$p.value), vars = c(etio_ng, T_ng))

# developed
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "1st_A4T"])$p.value), vars = c(etio_ng, T_ng))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "2nd_Con"])$p.value), vars = c(etio_ng, T_ng))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_Con"], .[inter == "2nd_A4T"])$p.value), vars = c(etio_ng, T_ng))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_A4T"], .[inter == "2nd_Con"])$p.value), vars = c(etio_ng, T_ng))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "1st_A4T"], .[inter == "2nd_A4T"])$p.value), vars = c(etio_ng, T_ng))
dapi_f %>%
        summarise_each(funs(t.test(.[inter == "2nd_Con"], .[inter == "2nd_A4T"])$p.value), vars = c(etio_ng, T_ng))

# pairwise undeveloped vs. developed
dapi_1c <- dapi[which(dapi$inter == "1st_Con"), ]
dapi_1a <- dapi[which(dapi$inter == "1st_A4T"), ]
dapi_2c <- dapi[which(dapi$inter == "2nd_Con"), ]
dapi_2a <- dapi[which(dapi$inter == "2nd_A4T"), ]

dapi_1c %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(etio_ng_g, T_ng_g))
dapi_1a %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(etio_ng_g, T_ng_g))
dapi_2c %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(etio_ng_g, T_ng_g))
dapi_2a %>%
        summarise_each(funs(t.test(.[develope == "undeveloped"], .[develope == "developed"])$p.value), vars = c(etio_ng_g, T_ng_g))


#================================================
#percentage plot
#===============================================
setwd("~/Desktop/pigeon Context-dependent 2019")
dp <- read_excel("percentage.xlsx", sheet = "R")
str(dp)
da <- mutate(dp,group = factor(group),
               hormone = factor(hormone), 
               develop = factor(develop))

dp$group <- factor(dp$group, levels = c("1C", "1A","2C","2A"))
dp$develop <- factor(dp$develop, levels = c("undeveloped", "developed"))
dp$hormone <- factor(dp$hormone, levels = c("androstenedione", "testosterone","conjugated testosterone","etiocholanolone","conjugated etiocholanolone","unknown"))

p1 <- ggplot(dp,aes(x=group, y = nmol, fill = hormone))+
        geom_bar(position = "stack", stat = "identity")+
        facet_wrap(~develop)+
        scale_fill_brewer(palette="Dark2")

p2 <- ggplot(dp,aes(x=group, y = nmol, fill = hormone))+
        geom_bar(position = "fill", stat = "identity")+
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
        facet_wrap(~develop)+
        scale_fill_brewer(palette="Dark2")+
        ylab("Percentage")+
        theme_classic()+
        theme(legend.title = element_text(size=18), #change legend title font size
              legend.text = element_text(size=18),#change legend text font size
              strip.text.x = element_text(size = 18))+ #change facet legend text font size
        font("xylab", size = 18)+
        font("xy.text", size = 18)

plot_grid(p1,p2,
          align = "hv", axis = "tb",
          ncol =1, nrow =2,greedy = T)


setwd("~/Desktop/pigeon Context-dependent 2019")
dper <- read_excel("percentage.xlsx", sheet = "percentage")
str(dper)
summary(dper)
dper <- mutate(dper,inter = factor(inter),
             egg_ID = factor(egg_ID), 
             nestID = factor(nestID),
             develope = factor(develope),
             treat = factor(treat),
             seq = factor(seq),
             T_nmol = as.numeric(T_nmol),
             conj_T_nmol = as.numeric(conj_T_nmol),
             etio_nmol = as.numeric(etio_nmol),
             conj_etio_nmol = as.numeric(conj_etio_nmol))
dper$inter <- factor(dper$inter, levels = c("1C", "1A","2C","2A"))
dper$treat <- factor(dper$treat, levels = c("Con", "A4T"))
dper <- dper[which(dper$develope == "developed"),]


my_comparisons <- list( c("1C", "1A"), c("1C","2C"),c("1A", "2C"), c("2C", "2A") )

pT<-ggplot(dper,aes(x=inter, y=(T_nmol/ave_allAndrogen_nmol)*100))+
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point( aes(fill = inter),position = position_dodge(0.8), size =3)+
        labs(x="group",y="Testosterone %")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           aes(label = ..p.signif.., size =2),
                           method="t.test")
pA4<-ggplot(dper,aes(x=inter, y=(A4_nmol/ave_allAndrogen_nmol)*100))+
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point( aes(fill = inter),position = position_dodge(0.8), size =3)+
        labs(x="group",y="Androstenedione %")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           aes(label = ..p.signif..),
                           method="t.test")
pconT<-ggplot(dper,aes(x=inter, y=(conj_T_nmol/ave_allAndrogen_nmol)*100, shape=inter))+
        scale_shape_manual(values=c(1,19,0,15)) +
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        labs(x=NULL,y="Conjugated testosterone %")+
        theme(legend.position = "none")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           #aes(label = ..p.signif..),
                           method="t.test"
                           #symnum.args=symnum.args
        )
pE<-ggplot(dper,aes(x=inter, y=(etio_nmol/ave_allAndrogen_nmol)*100,shape = inter))+
        scale_shape_manual(values=c(1,19,0,15)) +
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        labs(x=NULL,y="Etiocholanolone %")+
        theme(legend.position = "none")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           #aes(label = ..p.signif..),
                           method="t.test"
                           #symnum.args=symnum.args
        )
pconE<-ggplot(dper,aes(x=inter, y=(conj_etio_nmol/ave_allAndrogen_nmol)*100, shape = inter))+
        scale_shape_manual(values=c(1,19,0,15)) +
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        labs(x=NULL,y="Conjugated etiocholanolone %")+
        theme(legend.position = "none")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           #aes(label = ..p.signif..),
                           method="t.test"
                           #symnum.args=symnum.args
                           )

punknow<-ggplot(dper,aes(x=inter, y=((ave_allAndrogen_nmol-etio_nmol-T_nmol-A4_nmol-conj_etio_nmol-conj_T_nmol)/ave_allAndrogen_nmol)*100, shape=inter))+
        scale_shape_manual(values=c(1,19,0,15)) +
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        labs(x=NULL,y="Unknown metabolites %")+
        theme(legend.position = "none")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           #aes(label = ..p.signif..),
                           method="t.test"
                           #symnum.args=symnum.args
        )

plot_grid(convertA,NULL,pconT, pE,pconE,punknow,
          align = "hv", axis = "tb",labels = c("A","","B","C","D","E"),
          ncol =2, nrow =3,greedy = T,
          hjust = c(-7.5, -7.5,-7.5,-7.5,-7.5), vjust = 1)
#export with width = 900, height =1200

convertA<-ggplot(dper,aes(x=inter, y=((ave_T_nmol+ave_A4_nmol-A4_nmol-T_nmol)/ave_allAndrogen_nmol)*100, shape=inter))+
        scale_shape_manual(values=c(1,19,0,15)) +
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point(size= 3,alpha = 1,position = position_dodge(0.8),stroke = 1)+
        labs(x=NULL,y="Δ androgen %")+
        theme(legend.position = "none")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           #aes(label = ..p.signif..),
                           method="t.test"
                           #,symnum.args=symnum.args
                           )

symnum.args <- list(cutpoints = c(0, 0.0001/20, 0.001/20, 0.01/20, 0.0028, 1), symbols = c("****", "***", "**", "*", "ns"))#Bonferroni correction for multiple T-test

convertA4<-ggplot(dper,aes(x=inter, y=((ave_A4_nmol-A4_nmol)/ave_allAndrogen_nmol)*100, shape = inter))+
        scale_shape_manual(values=c(1,19,0,15)) +
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point( aes(fill = inter),position = position_dodge(0.8), size =3)+
        labs(x="group",y="Δ androstenedione %")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           aes(label = ..p.signif..),
                           method="t.test")
convertT<-ggplot(dper,aes(x=inter, y=((ave_T_nmol-T_nmol)/ave_allAndrogen_nmol)*100))+
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point( aes(fill = inter),position = position_dodge(0.8), size =3)+
        labs(x="group",y="Δ testosterone %")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           aes(label = ..p.signif..),
                           method="t.test")
convertconjT<-ggplot(dper,aes(x=inter, y=((conj_T_nmol-ave_conj_T_nmol)/ave_allAndrogen_nmol)*100))+
        stat_summary(fun.data = min.mean.sd.max, 
                     geom = "boxplot",
                     position = position_dodge(0.8),
                     lwd=0.75,width = 0.75)+
        theme_classic()+
        geom_point( aes(fill = inter),position = position_dodge(0.8), size =3)+
        labs(x="group",y="Δ conjugated testosterone %")+
        font("xylab", size = 18)+
        font("xy.text", size = 18)+
        stat_compare_means(comparisons = my_comparisons,
                           aes(label = ..p.signif..),
                           method="t.test")
plot_grid(convertA4,NULL, convertT,convertconjT,
          align = "hv", axis = "tb",labels = c("A", "","B","C"),
          ncol =2, nrow =2,greedy = T,
          hjust = c(-7.5, -7.5,-7.5,-7.5), vjust = 1)

