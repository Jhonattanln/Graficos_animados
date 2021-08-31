library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(gifski)
library(gganimate)
library(Rcpp)
library(tidyverse)


pl <- read_excel('economatica.xlsx')
ev <- read_excel('economatica_EV.xlsx')

pl <- transform(pl, CASN3 = as.numeric(CASN3))
ev <- transform(ev, CASN3 = as.numeric(CASN3))

pl$Data <- ymd(pl$Data)

class(pl$Data)

g_pl <- ggplot()+
  geom_line(pl, mapping = aes(x = Data, y = SAPR11, color = "Sanepar"), size = 1)+
  geom_line(pl, mapping = aes(x = Data, y = CSMG3, color = "Copasa"), size = 1)+
  geom_line(pl, mapping = aes(x = Data, y = SBSP3, color = "Sabesp"), size = 1)+
  guides(color = guide_legend('Empresa'))+
  theme(legend.title = element_text(size = 25), 
        legend.text = element_text(size = 20),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))+
  labs(caption = 'Fonte: Economatica')+
  ylab('Preço / Lucro')+
  xlab('Data')

g_ev <- ggplot()+
  geom_line(ev, mapping = aes(x = Data, y = SAPR11, color = "Sanepar"), size = 1)+
  geom_line(ev, mapping = aes(x = Data, y = CSMG3, color = "Copasa"), size = 1)+
  geom_line(ev, mapping = aes(x = Data, y = SBSP3, color = "Sabesp"), size = 1)+
  guides(color = guide_legend('Empresa'))+
  theme(legend.title = element_text(size = 25), 
        legend.text = element_text(size = 20),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))+
  labs(caption = 'Fonte: Economatica')+
  ylab('EV / Ebitda')+
  xlab('Data')

anim_ev <- g_ev + transition_reveal(Data)+
  labs(title = 'Dia: {frame_along}')

anim_pl <- g_pl + transition_reveal(Data)+
  labs(title = 'Dia: {frame_along}')

a <- animate(anim_pl, height = 566, width =1080)
b <- animate(anim_ev, height = 566, width =1080)

anim_save("grafico_pl.mp4", a)
anim_save("grafico_ev.mp4", b)
