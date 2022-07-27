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

pT <- ggplot(dper,aes(x=inter, y=(T_nmol/ave_allAndrogen_nmol)*100))+
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
pA4 <- ggplot(dper,aes(x=inter, y=(A4_nmol/ave_allAndrogen_nmol)*100))+
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
pconT <- ggplot(dper,aes(x=inter, y=(conj_T_nmol/ave_allAndrogen_nmol)*100, shape=inter))+
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
pE <- ggplot(dper,aes(x=inter, y=(etio_nmol/ave_allAndrogen_nmol)*100,shape = inter))+
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
pconE <- ggplot(dper,aes(x=inter, y=(conj_etio_nmol/ave_allAndrogen_nmol)*100, shape = inter))+
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

punknow <- ggplot(dper,aes(x=inter, y=((ave_allAndrogen_nmol-etio_nmol-T_nmol-A4_nmol-conj_etio_nmol-conj_T_nmol)/ave_allAndrogen_nmol)*100, shape=inter))+
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

