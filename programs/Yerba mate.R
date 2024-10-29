#---------------------------------------------------------------#
#                       Yerba mate
#     Ministerio de Desregulación y Transformación del Estado
#           Secretaria de Simplificación del Estado
#---------------------------------------------------------------#

#install.packages("showtext")
library(readxl)
library(lubridate)
library(ggplot2)
library(ggtext)
library(dplyr)
library(scales)
library(showtext)
library(writexl)

font_add_google("Montserrat", "montserrat")
showtext_auto()

setwd("direccion")

yerba_precio <- read_excel("IPCBA_base_2021100-Precios_medios_alim.xlsx", sheet = "Yerba mate")
yerba_precio <- yerba_precio %>%
  rename(Periodo = "Descripción")
yerba_precio$Periodo <- as.Date(yerba_precio$Periodo, format = "%Y-%m-%d")
names(yerba_precio) <- gsub(" ", "_", names(yerba_precio))

yerba_precio$Ponderar <- yerba_precio$Ponderar/100

#indico el año de corte que divide las tendencias en dos. Previo y posterior a la asunción de milei.
cutoff <- as.Date("2023-12-10")


#Graficamos:
ggplot(yerba_precio, aes(x = Periodo, y = Yerba_mate)) + 
  geom_line(color = "#46658B", size = 1) +
  geom_segment(aes(x = as.Date("2024-1-01"), xend = as.Date("2024-1-01"), 
                   y = 0, yend = 4500), 
               linetype = "dashed", color = "#E6B861", linewidth = 1) +
  geom_label(aes(x = as.Date("2023-09-01"), y = 3800, 
                 label = "DNU 70/2023"),
             color = "black", fill = "white", size = 3.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = 'transparent'), 
        plot.background = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        text = element_text(size = 14, family = "montserrat"),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 10)) + 
  labs(title = "Evolución del precio medio de la yerba mate",
       caption = "Fuente: Elaboración propia en base a datos del IPCBA") +
  ylab("Pesos") +
  xlab("Mes")


#### Gráfico variación ####

#Calculamos la variación porcentual mes a mes
yerba_precio <- yerba_precio %>%
  arrange(Periodo) %>% 
  mutate(variacion_mensual = (Yerba_mate - lag(Yerba_mate)) / lag(Yerba_mate) * 100)

ggplot(yerba_precio, aes(x = Periodo, y = variacion_mensual)) + 
  geom_line(color = "#46658B", size = 1) +
  geom_segment(aes(x = as.Date("2024-1-01"), xend = as.Date("2024-1-01"),                   y = -4, 
                   yend = 30), 
               linetype = "dashed", color = "#E6B861", linewidth = 1) +
  
  geom_label(aes(x = as.Date("2024-04-01"), 
                 y = max(variacion_mensual, na.rm = TRUE) * 0.95, 
                 label = "DNU 70/2023"),
             color = "black", fill = "white", size = 3.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  
  geom_hline(yintercept = 0, color = "#4D4D4D", linetype = "dashed", size = 0.8) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = 'transparent'), 
        plot.background = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        text = element_text(size = 14, family = "montserrat"),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 10)) + 
  labs(title = "Variación porcentual mensual del precio de la yerba mate",
       caption = "Las variaciones porcentuales se calculan mes a mes \n Fuente: Elaboración propia en base a datos del IPCBA") +
  ylab("Variación porcentual (%)") +
  xlab("Mes") +
  scale_y_continuous(limits = c(-10, 30), # Ajustamos los límites para ver la línea segmentada
                     breaks = seq(-10, 30, by = 5))

#### Gráfico precios constantes ####
yerba_precio$yerba_real <- yerba_precio$Yerba_mate/yerba_precio$Ponderar

((yerba_precio$yerba_real[30]-yerba_precio$yerba_real[22])/yerba_precio$yerba_real[22])*100
    
ggplot(yerba_precio, aes(x = Periodo, y = yerba_real)) + 
  geom_line(color = "#46658B", size = 1) +
  geom_segment(aes(x = as.Date("2024-1-01"), xend = as.Date("2024-1-01"),                   y = 350, yend = 600), 
               linetype = "dashed", color = "#E6B861", linewidth = 1) +
  geom_label(aes(x = as.Date("2023-10-01"), y = 550, 
                 label = "DNU 70/2023"),
             color = "black", fill = "white", size = 4.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = 'transparent'), 
        plot.background = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        text = element_text(size = 14, family = "montserrat"),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 10, angle=45),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(t = 5, r = 30, b = 5, l = 10)) + 
  labs(title = "Evolución del precio promedio a pesos constantes del kilo de yerba mate",
       caption = "Fuente: Elaboración propia en base a datos del IPCBA") +
  ylab("Pesos") +
  xlab("Mes") +
  geom_label(aes(x = as.Date("2024-06-01"), y = 500, 
                 label = "2024-08 vs. 2023-12\n-25.4%"),
             color = "black", fill = "white", size = 4.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  annotate(
    geom = "curve", x = as.Date("2024-06-01"), xend = as.Date("2024-08-01"), y = 487, yend = 400.67, curvature = 0, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches")))


yerba_precio_1 <- yerba_precio[yerba_precio$Periodo>="2023-01-01",]

ggplot(yerba_precio_1, aes(x = Periodo, y = yerba_real)) + 
  geom_line(color = "#46658B", size = 1) +
  geom_segment(aes(x = as.Date("2023-12-01"), xend = as.Date("2023-12-01"),                   y = 350, yend = 600), 
               linetype = "dashed", color = "#E6B861", linewidth = 1) +
  geom_label(aes(x = as.Date("2023-10-01"), y = 550, 
                 label = "DNU 70/2023"),
             color = "black", fill = "white", size = 4.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = 'transparent'), 
        plot.background = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        text = element_text(size = 14, family = "montserrat"),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 10, angle=45),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(t = 5, r = 30, b = 5, l = 10)) + 
  labs(title = "Evolución del precio promedio a pesos constantes del kilo de yerba mate",
       caption = "Fuente: Elaboración propia en base a datos del IPCBA") +
  ylab("Pesos") +
  xlab("Mes") +
  geom_label(aes(x = as.Date("2024-06-01"), y = 500, 
                 label = "2024-08 vs. 2023-12\n-25.4%"),
             color = "black", fill = "white", size = 4.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  annotate(
    geom = "curve", x = as.Date("2024-06-01"), xend = as.Date("2024-08-01"), y = 487, yend = 400.67, curvature = 0, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches")))






#### Gráfico precios constantes sin estacionalidad ####


yerba_precio$mes <- format(yerba_precio$Periodo, "%m")
yerba_precio$mes <- factor(yerba_precio$mes, levels = sprintf("%02d", 1:12))
yerba_precio <- yerba_precio %>%
  arrange(mes)

modelo <- lm(yerba_real ~ factor(mes), data = yerba_precio)
summary(modelo)
yerba_precio$residuos <- residuals(modelo)

yerba_precio$residuos_normalizados <- scale(yerba_precio$residuos)


ggplot(yerba_precio, aes(x = Periodo, y = residuos)) + 
  geom_line(color = "#46658B", size = 1) +
  geom_segment(aes(x = as.Date("2024-01-01"), xend = as.Date("2024-1-01"), y = -100, yend = 100), 
               linetype = "dashed", color = "#E6B861", linewidth = 1) +
  geom_label(aes(x = as.Date("2023-11-01"), y = 65, 
                 label = "DNU 70/2023"),
             color = "black", fill = "white", size = 4.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = 'transparent'), 
        plot.background = element_rect(fill = 'transparent', color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        text = element_text(size = 14, family = "montserrat"),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 10)) + 
  labs(title = "Evolución del precio medio a pesos constantes de la yerba mate sin estacionalidad",
       caption = "Fuente: Elaboración propia en base a datos del IPCBA") +
  ylab("Pesos") +
  xlab("Mes") + 
  geom_label(aes(x = as.Date("2024-06-01"), y = 38, 
                 label = "2024-08 vs. 2023-12\n-25.4%"),
             color = "black", fill = "white", size = 4.5, 
             family = "montserrat", label.size = 0.5, label.padding = unit(0.25, "lines")) +
  annotate(
    geom = "curve", x = as.Date("2024-06-01"), xend = as.Date("2024-08-01"), y = 30, yend = -40, curvature = 0, 
    angle = 60, color = "#E6B861", linewidth = .4, 
    arrow = arrow(type = "closed", length = unit(.08, "inches")))


prueba <- yerba_precio[yerba_precio$Periodo%in%c("2023-12-01","2024-08-01"),]


#Guardamos la base de datos para las estimaciones:
write_xlsx(yerba_precio, "C:/Users/Usuario/Desktop/Trabajo - UdeSA/Trabajo - Ministerio/Yerba/input/yerba_precio_est.xlsx")






