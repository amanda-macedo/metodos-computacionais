setwd("~/GitHub/metodos-computacionais/T1/Tentativa 1/Q2")
library(dplyr)
load(file = "T1_q2_c.rda")
data_q2['media'] = (data_q2$bola_1 + data_q2$bola_2) / 2
media_2_3 = data_q2 %>% filter(media==2|media==3) %>% nrow()
round(media_2_3/nrow(data_q2)*1000,3)
