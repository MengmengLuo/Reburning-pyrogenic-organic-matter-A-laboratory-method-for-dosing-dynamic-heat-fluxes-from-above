#This code is for 
# 1. graphing the calibration results for high and low heat flux profiles
# 2. graphing programmed high and low heat flux profiles (Figure.2)
#the graphs are presented in the SI
#Mengmeng Luo

#importing measured data

highHF22 = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/raw/blankhigh_10_22.csv")
lowHF22 = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/raw/blanklow_10_22.csv")
highHF24.1 = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/raw/blankhigh_02_24.csv")
lowHF24.1 = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/raw/blanklow_02_24.csv")
highHF24.2 = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/raw/blankhigh_10_24.csv")
lowHF24.2 = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/raw/blanklow_10_24.csv")

#take out additional rows and columns and rename to prepare for dataset combine

highHF22 = highHF22[-c(1:4,16987:17212),-c(1:3,6,7,8:20)]
lowHF22 = lowHF22[-c(1:8,9010:9671),-c(1:3,6,7,8:20)]

highHF22$Oct_2022 = highHF22$heat.flux..kW.m..
lowHF22$Oct_2022 = lowHF22$heat.flux..kW.m..


highHF24.1 = highHF24.1[,-c(2,3,6,7,8:21)]
lowHF24.1 = lowHF24.1[,-c(2,3,6,7,8:21)]

highHF24.1$Feb_2024 = highHF24.1$heat.flux..kW.m..
lowHF24.1$Feb_2024 = lowHF24.1$heat.flux..kW.m..

highHF24.2 =highHF24.2[,-c(2,3,6,7,8:21)]
lowHF24.2 = lowHF24.2[,-c(2,3,6,7,8:21)]

highHF24.2$Oct_2024 = highHF24.2$heat.flux..kW.m..
lowHF24.2$Oct_2024 = lowHF24.2$heat.flux..kW.m..

df.high = cbind(highHF22, highHF24.1, highHF24.2)
df.low = cbind(lowHF22, lowHF24.1, lowHF24.2)

df.high = df.high[,-c(1,2,5,8,9,10)]
df.low = df.low[,-c(1,2,5,8,9,10)]

df.high$hours = df.high$Time..sec./3600
df.low$hours = df.low$Time..sec./3600


#graphing high HF

library(ggeasy)
library(ggpubr)
library(grid)
library(gridExtra)

colors = c('Setpoint' = 'black', 'Run 1' = 'grey40', 'Run 2' = 'grey60', 'Run 3' = 'grey80')
linetypes = c('Setpoint' = 'solid',  'Measurement' = 'dashed')
fills =  c('Setpoint' = 'black', 'OCT2022' = 'grey40', 'FEB2024' = 'grey60', 'OCT2024' = 'grey80')

p2 = ggplot()+
  geom_line(data = df.high, aes(x = hours, y = Oct_2022, col = 'Run 1'), size = 1)+
  geom_line(data = df.high, aes(x = hours, y = Feb_2024, col = 'Run 2'), size = 1)+
  geom_line(data = df.high, aes(x = hours, y = Oct_2024, col = 'Run 3'), size = 1)+
  geom_line(data = df.high, aes(x = hours, y = HF.setpoint..kW.m.., col = 'Setpoint'), size = 1)+
  labs(x = expression ("Time"~(h)), y = expression ("Heat Flux"~(kW~m^-2)), color = "Legend")+
  scale_color_manual(name = "Heat Flux",
                     breaks = c('Setpoint', 'Run 1', 'Run 2', 'Run 3'),
                     values = colors)+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.75)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95))+
  ggtitle("High Heat Flux")+
  ggeasy::easy_center_title()
p2

#graphing low HF

p3 = ggplot()+
  geom_line(data = df.low, aes(x = hours, y = Oct_2022, col = 'Run 1'), size = 1)+
  geom_line(data = df.low, aes(x = hours, y = Feb_2024, col = 'Run 2'), size = 1)+
  geom_line(data = df.low, aes(x = hours, y = Oct_2024, col = 'Run 3'), size = 1)+
  geom_line(data = df.low, aes(x = hours, y = HF.setpoint..kW.m.., col = 'Setpoint'), size = 1)+
  labs(x = expression ("Time"~(h)), y = expression ("Heat Flux"~(kW~m^-2)), color = "Legend")+
  scale_color_manual(name = "Heat Flux",
                     breaks = c('Setpoint', 'Run 1', 'Run 2', 'Run 3'),
                     values = colors)+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.6)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 55))+
  ggtitle("Low Heat Flux")+
  ggeasy::easy_center_title()
p3


#combined
grid.arrange(p2, p3, ncol = 2)

ggarrange(p2, p3,
          common.legend = TRUE, legend = "bottom")
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/code/HF_profiles_comparison.tiff", units="in", width=8, height=4)
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/code/HF_profiles_comparison_newdata.tiff", units="in", width=8, height=4)

#graphing delta

df.high$delta2022 = df.high$Oct_2022-df.high$HF.setpoint..kW.m..
df.high$delta202402 = df.high$Feb_2024-df.high$HF.setpoint..kW.m..
df.high$delta202410 = df.high$Oct_2024-df.high$HF.setpoint..kW.m..

df.low$delta2022 = df.low$Oct_2022-df.low$HF.setpoint..kW.m..
df.low$delta202402 = df.low$Feb_2024-df.low$HF.setpoint..kW.m..
df.low$delta202410 = df.low$Oct_2024-df.low$HF.setpoint..kW.m..

p4 = ggplot()+
  geom_point(data = df.high, aes(x = hours, y = delta2022, col = 'Run 1'), size = 1)+
  geom_point(data = df.high, aes(x = hours, y = delta202402, col = 'Run 2'), size = 1)+
  geom_point(data = df.high, aes(x = hours, y = delta202410, col = 'Run 3'), size = 1)+
  labs(x = expression ("Time"~(h)), y = expression (Delta*"Heat Flux"~(kW~m^-2)))+
  scale_color_manual(name = expression (Delta*"Heat Flux"),
                     breaks = c('Run 1', 'Run 2', 'Run 3'),
                     values = colors)+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.75)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-7.5, 7.5))+
  ggtitle("High Heat Flux")+
  ggeasy::easy_center_title()
p4

p6 = ggplot()+
  geom_point(data = df.low, aes(x = hours, y = delta2022, col = 'Run 1'), size = 1)+
  geom_point(data = df.low, aes(x = hours, y = delta202402, col = 'Run 2'), size = 1)+
  geom_point(data = df.low, aes(x = hours, y = delta202410, col = 'Run 3'), size = 1)+
  labs(x = expression ("Time"~(h)), y = expression (Delta*"Heat Flux"~(kW~m^-2)))+
  scale_color_manual(name = expression (Delta*"Heat Flux"),
                     breaks = c('Run 1', 'Run 2', 'Run 3'),
                     values = colors)+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.51)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-7.5, 7.5))+
  ggtitle("Low Heat Flux")+
  ggeasy::easy_center_title()
p6

grid.arrange(p4, p6, ncol = 2)
ggarrange(p4, p6,
          common.legend = TRUE, legend = "bottom")

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/code/HF_profiles_delta.tiff", units="in", width=8, height=4)
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/code/HF_profiles_delta_newdata.tiff", units="in", width=8, height=4)


p5 = ggplot()+
  geom_point(data = df.low, aes(x = hours, y = delta2022, col = 'Run 1'), size = 1,show.legend = FALSE)+
  geom_point(data = df.low, aes(x = hours, y = delta202402, col = 'Run 2'), size = 1,show.legend = FALSE)+
  geom_point(data = df.low, aes(x = hours, y = delta202410, col = 'Run 3'), size = 1,show.legend = FALSE)+
  labs(x = expression ("Time"~(h)), y = expression (Delta*"Heat Flux"~(kW~m^-2)))+
  scale_color_manual(values = colors)+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend = NULL,
        #axis.line = element_line(color='black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.55)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-50, 50))
p5

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/code/HF_profiles_delta_lowHF_full.tiff", units="in", width=4, height=3)
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/code/HF_profiles_delta_lowHF_full_newdata.tiff", units="in", width=4, height=2)


#write.csv(df.high,"~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/highHF_calibration.csv", row.names = FALSE)
#write.csv(df.low,"~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/data/derived/lowHF_calibration.csv", row.names = FALSE)

#programmed high and low heat flux profiles (Figure 2 in the paper)
colors = c('High' = '#debb6a', 'Low' = '#58822a')
p1 = ggplot()+
  geom_line(data = df.high, aes(x = hours, y = HF.setpoint..kW.m.., col = "High"), size = 1)+
  geom_line(data = df.low, aes(x = hours, y = HF.setpoint..kW.m.., col = "Low"), size = 1)+
  labs(x = expression ("Time"~(h)), y = expression ("Heat Flux"~(kW~m^-2)), color = "Legend")+
  scale_color_manual(name = "Heat Flux",
                     breaks = c('High', 'Low' ),
                     values = colors)+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5), breaks = seq(0, 5, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 90))
p1

