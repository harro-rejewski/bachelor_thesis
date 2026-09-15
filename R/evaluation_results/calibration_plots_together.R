#plots for all forecasts for marginal, probabilistic, quantile Calibration ####
#needs the results from the forecasts
#1. Marginal Calibration plots ####
#24h
png("plots/marg_24_Rplot.png", width=1600, height=600,res = 150)
print((marg_emp_24 + labs(
  subtitle = "Empirische Vorhersage 24h",
  x = "",
  y = "empirische Verteilungsfunktion\n der Temperatur"
) +
  marg_emos_glob_24 + labs(
    subtitle = "EMOS global 24h",
    x = "mittlere Vorhersageverteilungsfunktion",
    y = ""
  ) +
  marg_emos_lok_24 + labs(
    subtitle = "EMOS lokal 24h",
    x = "",
    y = ""
  )) &
  theme_bw(base_size = 15))
dev.off()

#48h  
png("plots/marg_48_Rplot.png", width=1600, height=600,res = 150)
print((marg_emp_48 + labs(
  subtitle = "Empirische Vorhersage 48h",
  x = "",
  y = "empirische Verteilungsfunktion\n der Temperatur"
) +
  marg_emos_glob_48 + labs(
    subtitle = "EMOS global 48h",
    x = "mittlere Vorhersageverteilungsfunktion",
    y = ""
  ) +
  marg_emos_lok_48 + labs(
    subtitle = "EMOS lokal 48h",
    x = "",
    y = ""
  )) &
  theme_bw(base_size = 15))
dev.off()  


#2. Probabilisitc calibration ####
#24h
png("plots/pit_24_Rplot.png", width=1600, height=900,res = 150)
print(
(pit_emp_24 + labs(subtitle = "Empirische Vorhersage 24h")+
 pit_emos_glob_24 + labs(subtitle = "EMOS global 24h")+
 pit_emos_lok_24 +labs(subtitle = "EMOS lokal 24h") &
   scale_y_continuous(limits = c(0, 1)) &
   scale_x_continuous(limits = c(0, 1))
   ) / 
  (pit_hist_emp_24 + pit_hist_emos_glob_24 + pit_hist_emos_lok_24 &
     scale_y_continuous(limits = c(0, 3.8)) &
     geom_hline(yintercept = 1,linetype = "dashed",color = "grey40"))&
  theme_bw(base_size = 15))
dev.off()  


#48h
png("plots/pit_48_Rplot.png", width=1600, height=900,res = 150)
print((pit_emp_48 +labs(subtitle = "Empirische Vorhersage 48h")+ #labs(title = "PIT-Reliabilitätsdiagramm und Histogramm")+
    pit_emos_glob_48 + labs(subtitle = "EMOS global 48h")+
    pit_emos_lok_48 +labs(subtitle = "EMOS lokal 48h") &
    scale_y_continuous(limits = c(0, 1)) &
    scale_x_continuous(limits = c(0, 1))) / 
  (pit_hist_emp_48 + pit_hist_emos_glob_48 + pit_hist_emos_lok_48 &
     scale_y_continuous(limits = c(0, 3.8)) &
     geom_hline(yintercept = 1,linetype = "dashed",color = "grey40"))&
  theme_bw(base_size = 15))
dev.off()  


#3. Quantilcalibration ####
#all 25,50, 75 24h
png("plots/quant_24_Rplot.png", width=4000, height=4500,res = 225)
print((q_25_emp_24+ labs(subtitle = "Empirische Vorhersage 24h")+
  emos_glob_q25_24+labs(subtitle = "EMOS global 24h")+
   (emos_lok_q25_24+labs(subtitle = "EMOS lokal 24h"))) /
  (q_50_emp_24+
  emos_glob_q50_24+
  emos_lok_q50_24) /
  (q_75_emp_24+
    emos_glob_q75_24+
    emos_lok_q75_24) &
  scale_y_continuous(breaks=seq(-5,15,by=3))&
  scale_x_continuous(breaks=seq(-6,12,by=3))&
  theme_bw(base_size = 25))
dev.off()  

#all 25,50, 75 48h
png("plots/quant_48_Rplot.png", width=4000, height=4500,res = 225)
print((q_25_emp_48+ labs(subtitle = "Empirische Vorhersage 48h")+
    emos_glob_q25_48+labs(subtitle = "EMOS global 48h")+
    (emos_lok_q25_48+labs(subtitle = "EMOS lokal 48h"))) /
  (q_50_emp_48+
     emos_glob_q50_48+
     emos_lok_q50_48) /
  (q_75_emp_48+
     emos_glob_q75_48+
     emos_lok_q75_48) &
    scale_y_continuous(breaks=seq(-5,15,by=3))&
    scale_x_continuous(breaks=seq(-6,12,by=3))&
    theme_bw(base_size = 20))
dev.off()  
