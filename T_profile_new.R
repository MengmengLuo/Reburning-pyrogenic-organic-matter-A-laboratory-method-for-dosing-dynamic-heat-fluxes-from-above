#The code is for graphing temperature profiles (Figure. 6, Figure. S7)
# Mengmeng Luo

highHFsurface = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/highHFsurface.csv")
highHF1cm = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/highHF1cm.csv")
highHF5cm = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/highHF5cm.csv")
lowHFsurface = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/lowHFsurface.csv")
lowHF1cm = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/lowHF1cm.csv")
lowHF5cm = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/lowHF5cm.csv")

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpubr)
library(grid)
library(gridExtra)


# HF setpoint
T_HighHF_surface = highHFsurface[,c(5,8:18)]
T_HighHF_1cm = highHF1cm[,c(8:18)]
T_HighHF_5cm = highHF5cm[,c(8:18)]
T_HighHF_surf_1cm = merge(T_HighHF_surface, T_HighHF_1cm, by="hours")
T_HighHF = merge(T_HighHF_surf_1cm, T_HighHF_5cm, by="hours")

HFplot = T_HighHF$HF_Setpoint*10 # plot the HF using log10
T_HighHF = cbind(T_HighHF, HFplot)


T_LowHF_surface = lowHFsurface[,c(5,8:18)]
T_LowHF_1cm = lowHF1cm[,c(8:18)]
T_LowHF_5cm = lowHF5cm[,c(8:18)]
T_LowHF_surf_1cm = merge(T_LowHF_surface, T_LowHF_1cm, by="hours")
T_LowHF = merge(T_LowHF_surf_1cm, T_LowHF_5cm, by="hours")

HFplot1 = T_LowHF$HF_Setpoint*10
T_LowHF = cbind(T_LowHF, HFplot1)

# plot T profiles (bottom only, Figure. 6)

coeff = 0.1
colors = c('Surface' = '#a62445', '1 cm' = '#d4ad2c', '5 cm' = '#2d8ab3', 'HF Setpoint' = 'black' )

library(ggeasy)

plot_highHF = ggplot(data = T_HighHF, aes(x = hours))+
  geom_line(aes(y = HFplot, col = 'HF Setpoint'), size = 1.5)+
  geom_line(aes(y = TC6, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC7, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC8, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC9, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC10, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC16, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC17, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC18, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC19, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC20, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC26, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC27, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC28, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC29, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC30, col = '5 cm'), size = 0.8)+
  labs(x = "Time (hrs)", y = "Temperature(°C)", color = "Depth")+
  scale_color_manual(values = colors)+
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))+
  theme_bw() +
  theme(#axis.line = element_line(color='black'),
    #panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.72), breaks = seq(0, 5, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000))+
  ggtitle("High Heat Flux")+
  ggeasy::easy_center_title()

plot_highHF = plot_highHF+
  #geom_line(aes(y = HFplot,group = 1, col = 'black'), size=1)+
  scale_y_continuous(
    name = expression("Temperature ("~degree~"C)"), 
    sec.axis = sec_axis(~ . *0.1 , name = expression ("Heat Flux"~(kW/m^2))),expand = c(0, 0),limits = c(0, 900))

plot_highHF

plot_lowHF = ggplot(data = T_LowHF, aes(x = hours))+
  geom_line(aes(y = HFplot1, col = 'HF Setpoint'), size = 1.5)+
  geom_line(aes(y = TC36, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC37, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC38, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC39, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC40, col = 'Surface'), size = 0.8)+
  geom_line(aes(y = TC46, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC47, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC48, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC49, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC50, col = '1 cm'), size = 0.8)+
  geom_line(aes(y = TC56, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC57, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC58, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC59, col = '5 cm'), size = 0.8)+
  geom_line(aes(y = TC60, col = '5 cm'), size = 0.8)+
  labs(x = "Time (hrs)", y = "Temperature(°C)", color = "Depth")+
  scale_color_manual(values = colors)+
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))+
  theme_bw() +
  theme(#axis.line = element_line(color='black'),
    # panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.5), breaks = seq(0, 2.5, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000))+
  ggtitle("Low Heat Flux")+
  ggeasy::easy_center_title()

plot_lowHF = plot_lowHF+
  #geom_line(aes(y = HFplot1,group = 1, col = 'black'), size=1)+
  scale_y_continuous(
    name = expression("Temperature ("~degree~"C)"), 
    sec.axis = sec_axis(~ . *0.1 , name = expression ("Heat Flux"~(kW/m^2))),expand = c(0, 0),limits = c(0, 900))

plot_lowHF

grid.arrange(plot_highHF, plot_lowHF, ncol = 2)

ggarrange(plot_highHF, plot_lowHF,
          common.legend = TRUE, legend = "bottom")


##############################
#plot the full T profile (Figure. S7)
colors = c('Surface (Top)' = '#faa7c4','Surface (Bottom)' = '#a62445', '1 cm (Top)' = '#fcea8b','1 cm (Bottom)' = '#d4ad2c','5 cm (Top)' = '#7884f0', '5 cm (Bottom)' = '#2d8ab3', 'HF Setpoint' = 'black' )

plot_highHF = ggplot(data = T_HighHF, aes(x = hours))+
  geom_line(aes(y = TC1, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC2, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC3, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC4, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC5, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC6, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC7, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC8, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC9, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC10, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC11, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC12, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC13, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC14, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC15, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC16, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC17, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC18, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC19, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC20, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC21, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC22, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC23, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC24, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC25, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC26, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC27, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC28, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC29, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC30, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = HFplot, col = 'HF Setpoint'), size = 1.5)+
  labs(x = "Time (hrs)", y = "Temperature(°C)", color = "Depth")+
  scale_color_manual(values = colors)+
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))+
  theme_bw() +
  theme(#axis.line = element_line(color='black'),
    #panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.72), breaks = seq(0, 5, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000))+
  ggtitle("High Heat Flux")+
  ggeasy::easy_center_title()

plot_highHF = plot_highHF+
  #geom_line(aes(y = HFplot,group = 1, col = 'black'), size=1)+
  scale_y_continuous(
    name = expression("Temperature ("~degree~"C)"), 
    sec.axis = sec_axis(~ . *0.1 , name = expression ("Heat Flux"~(kW/m^2))),expand = c(0, 0),limits = c(0, 900))

plot_highHF

plot_lowHF = ggplot(data = T_LowHF, aes(x = hours))+
  geom_line(aes(y = TC31, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC32, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC33, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC34, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC35, col = 'Surface (Top)'), size = 0.8)+
  geom_line(aes(y = TC36, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC37, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC38, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC39, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC40, col = 'Surface (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC41, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC42, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC43, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC44, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC45, col = '1 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC46, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC47, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC48, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC49, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC50, col = '1 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC51, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC52, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC53, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC54, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC55, col = '5 cm (Top)'), size = 0.8)+
  geom_line(aes(y = TC56, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC57, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC58, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC59, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = TC60, col = '5 cm (Bottom)'), size = 0.8)+
  geom_line(aes(y = HFplot1, col = 'HF Setpoint'), size = 1.5)+
  labs(x = "Time (hrs)", y = "Temperature(°C)", color = "Depth")+
  scale_color_manual(values = colors)+
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))+
  theme_bw() +
  theme(#axis.line = element_line(color='black'),
    #panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.5), breaks = seq(0, 2.5, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000))+
  ggtitle("Low Heat Flux")+
  ggeasy::easy_center_title()

plot_lowHF = plot_lowHF+
  #geom_line(aes(y = HFplot1,group = 1, col = 'black'), size=1)+
  scale_y_continuous(
    name = expression("Temperature ("~degree~"C)"), 
    sec.axis = sec_axis(~ . *0.1 , name = expression ("Heat Flux"~(kW/m^2))),expand = c(0, 0),limits = c(0, 900))

plot_lowHF

grid.arrange(plot_highHF, plot_lowHF, ncol = 2)

ggarrange(plot_highHF, plot_lowHF,
          common.legend = TRUE, legend = "bottom")

