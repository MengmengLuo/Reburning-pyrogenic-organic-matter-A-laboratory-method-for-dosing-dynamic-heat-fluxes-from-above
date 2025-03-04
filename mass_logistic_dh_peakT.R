# This code is for 
# 1. plotting mass remaining and logistic models (Figure 7 and Figure 8)
# 2. getting statistics for degree hours and peak temperature (Table. 1, Table. S1, Table. S2)
# Mengmeng Luo

#plot mass remaining
setwd("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/mass")
mass = read.csv("mass_for_plot.csv")
below_surface = c(0, 1, 5,0,1,5,0,1,5)
class(below_surface)
mass = cbind(mass, below_surface)

library(ggplot2)
library(dplyr)
library(vegan)

colors = c('High' = '#debb6a', 'Low' = '#58822a', 'Control' = '#D3D3D3')
#mass$below_surface = factor(mass$below_surface, levels = c('5','1', '0'), ordered = TRUE)
mass$HeatFlux = factor(mass$HeatFlux, levels = c('High','Low','Control'), ordered = TRUE)

mass %>%
  ggplot(aes(x = MassRemain,y = below_surface))+
  geom_vline(xintercept=0,color="lightgrey")+geom_hline(yintercept=0, color="lightgrey")+
  geom_line(aes(linetype=HeatFlux), size = 1)+
  geom_point(aes(fill=HeatFlux, shape=HeatFlux), size = 5)+
  scale_fill_manual(values = c('#debb6a', '#58822a','#D3D3D3'))+
  scale_shape_manual(values=c(24,21,23))+
  guides(fill=guide_legend(title="Heat Flux"),shape=guide_legend(title="Heat Flux"),linetype=guide_legend(title="Heat Flux"))+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #panel.background = element_rect(colour = "black", size=1))+
  labs(x = "Mass Remaining (%)", y = "Depth (cm)", legend = "Depth")+
  scale_y_reverse(limits=c(5.5,-0.5), breaks = seq(0, 6, 1)) +
  scale_x_continuous(position="top",expand = c(0, 0), limits = c(-9, 105), breaks = seq(0, 100, 25))

#prepare dataset for logistic model fitting
mass$frac.mass.loss = paste((100-mass$MassRemain)/100)
mass = mass[-c(7,8,9),]

Heat.Flux = c("High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", 
              "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low")
mass = cbind(mass, Heat.Flux)

Depth = c("Surface", "Surface", "Surface", "Surface", "Surface", "1 cm", "1 cm", "1 cm", "1 cm", "1 cm", "5 cm", "5 cm", "5 cm", "5 cm", "5 cm", 
          "Surface", "Surface", "Surface", "Surface", "Surface", "1 cm", "1 cm", "1 cm", "1 cm", "1 cm", "5 cm", "5 cm", "5 cm", "5 cm", "5 cm")
mass = cbind(mass, Depth)

frac.mass.loss = c(0.98863862866, 0.98863862866, 0.98863862866, 0.98863862866, 0.98863862866,
                   0.98445595855, 0.98445595855, 0.98445595855, 0.98445595855, 0.98445595855,
                   0.2919416117, 0.2919416117, 0.2919416117, 0.2919416117, 0.2919416117,
                   0.93512214658, 0.93512214658, 0.93512214658, 0.93512214658, 0.93512214658,
                   0.4772364217, 0.4772364217, 0.4772364217, 0.4772364217, 0.4772364217, 
                   0.0660396238, 0.0660396238, 0.0660396238, 0.0660396238, 0.0660396238)
mass = cbind(mass, frac.mass.loss)

mass = mass[, -c(1:6)]

Sample.ID = c(1:30)
mass = cbind(mass, Sample.ID)



###get temperature dataset
summaryT = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/degree_hours/data/Summary_T_New.csv")
bottom_only = summaryT[-c(1:5, 11:15, 21:25, 31:35, 41:45, 51:55),]
Sample.ID = c(1:30)
bottom_only = cbind(bottom_only, Sample.ID)

merge.T = merge(mass, bottom_only,by="Sample.ID")
merge.T$Depth = merge.T$Depth.x
colors = c('Surface' = '#a62445', '1 cm' = '#d4ad2c', '5 cm' = '#2d8ab3' )


# fit mass loss fraction vs. peak temperature

Model.logi.p = glm(frac.mass.loss~Peak_T,data=merge.T,family = "binomial", na.action = na.exclude)
summary(Model.logi.p)

#get R-squared
library(pscl)
pR2(Model.logi.p)
1 - (Model.logi.p$deviance / Model.logi.p$null.deviance)^(2 / nrow(merge.T))

p4 = ggplot(merge.T)+
  geom_point(aes(x=Peak_T,y=frac.mass.loss,fill=Depth, color = Depth, shape=Heat.Flux.y))+
  theme_bw()+
  stat_smooth(aes(x=Peak_T, y=frac.mass.loss), method="glm",method.args = list(family=binomial), se=FALSE, col = "black", size = 0.5)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Peak Temperature (°C)", y = "Mass Loss Fraction", color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(#panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 660))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))
# geom_label(aes(x = 400, y = 0.2), hjust = 0, 
#           label = paste( "y = 0.002x - 0.15",
#                         "\nRsq = 0.85",
#                        "\np < 0.05"),
#        label.size = NA)
p4

# fit mass loss fraction vs. degree hours
Model.logi.df = glm(frac.mass.loss~Degree.hours,data=merge.T,family = "binomial", na.action = na.exclude)
summary(Model.logi.df)

#get R-squared
library(pscl)
pR2(Model.logi.df)
1 - (Model.logi.df$deviance / Model.logi.df$null.deviance)^(2 / nrow(merge.T))

p5 = ggplot(merge.T)+
  geom_point(aes(x=Degree.hours,y=frac.mass.loss,fill=Depth, color = Depth, shape=Heat.Flux.y))+
  stat_smooth(aes(x=Degree.hours, y=frac.mass.loss), method="glm",method.args = list(family=binomial), se=FALSE, col = "black", size = 0.5)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Degree Hours (°C hrs)", y = "Mass Loss Fraction", color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(#panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2200))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))
#geom_label(aes(x = 1200, y = 0.2), hjust = 0, 
#          label = paste( "y = 0.0004x + 0.20",
#                        "\nRsq = 0.54",
#                       "\np < 0.05"),
#       label.size = NA)
p5

library(gridExtra)
library(ggpubr)
grid.arrange(p5, p4, ncol = 2)
ggarrange(p5, p4,
          common.legend = TRUE, legend = "bottom")


#################################################

###statistics for degree hours and peak temperature (Table. 1)
df.peak <- bottom_only %>%
  group_by(Heat.Flux, Depth) %>%
  summarize(
    sd = sd(Peak_T),
    PeakT = mean(Peak_T)) %>%
  as.data.frame()
df.peak

df.degreehours<- bottom_only %>%
  group_by(Heat.Flux, Depth) %>%
  summarize(
    sd = sd(Degree.hours),
    dh = mean(Degree.hours)) %>%
  as.data.frame()
df.degreehours

anova1 <- aov(Degree.hours ~ Heat.Flux * Depth, data = bottom_only)
summary(anova1)

anova2 <- aov(Peak_T ~ Heat.Flux * Depth, data = bottom_only)
summary(anova2)

library(multcompView)

tk = TukeyHSD(anova1)
cld <- multcompLetters4(anova1, tk)
tk
cld

tk = TukeyHSD(anova2)
cld <- multcompLetters4(anova2, tk)
tk
cld
