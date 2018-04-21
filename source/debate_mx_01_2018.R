rm(list=ls())
setwd('~')
wd<-list.dirs(path = 'Documents', full.names = T)[grep('debate', list.dirs(path = 'Documents', full.names = T))][1]
setwd(wd)
tokens<-read.csv('./source/tokens.csv', stringsAsFactors = F)

library(rtweet)
library(data.table)

Sys.setenv(TZ='America/Mexico_City')
token_1<-create_token(app = tokens$app[1],
                    consumer_key = tokens$consumer_key[1], 
                    consumer_secret = tokens$consumer_secret[1])


data<-NULL
data_cons<-data.table(hora=.POSIXct(Sys.time(), 'America/Mexico_City'), n_tweets=NA, n_amlo=NA,
                      n_meade=NA, n_anaya=NA, n_bronco=NA, n_maza=NA)
data_i<-data.table(hora=NA, n_tweets=NA, n_amlo=NA,
                      n_meade=NA, n_anaya=NA, n_bronco=NA, n_maza=NA)
horas<-4

for(i in 1:(60*horas)){
  if(i %% 60==0) {
  message(paste('Ya son las', Sys.time()))
  }
data<-stream_tweets(
  "@lopezobrador_, @JoseAMeadeK, @JaimeRdzNL, @RicardoAnayaC, @Mzavalagc",
  timeout = 60,
  file_name = 'PRUEBA.json',
  parse = TRUE, verbose = F)
write_as_csv(data, paste0('./data/tw_', i,'.csv'))
#####RESUMEN#####
data_i$hora<-.POSIXct(Sys.time(), 'America/Mexico_City')
data_i$n_tweets<-nrow(data)
data_i$n_amlo<-sum(grepl('lopezobrador_', data$text))
data_i$n_meade<-sum(grepl('JoseAMeadeK', data$text))
data_i$n_anaya<-sum(grepl('RicardoAnayaC', data$text))
data_i$n_maza<-sum(grepl('Mzavalagc', data$text))
data_i$n_bronco<-sum(grepl('JaimeRdzNL', data$text))
data_cons<-rbind(data_cons, data_i)


##plotea menciones caa 5 minutos##
if (i %%5 ==0){
  message('actuaizando gráficas...')
plot_menciones<-ggplot(data=data_cons[-1,], aes(x=hora, y=n_tweets))+
  geom_line(color='grey')+
  geom_line(aes(x=hora, y=n_amlo), color='#AC2C2C')+
  geom_line(aes(x=hora, y=n_meade), color='red')+
  geom_line(aes(x=hora, y=n_anaya), color='blue')+
  geom_line(aes(x=hora, y=n_bronco), color='#2D3672')+
  geom_line(aes(x=hora, y=n_maza), color='#B917BF')+
  theme_light()+
  ylab('Número de menciones')+
  xlab('Hora')+
  ggtitle('Número de menciones por minuto', subtitle = 'Gris:Número de menciones total \n Marrón:@lopezobrador_ \n Rojo:@JoseAMeadeK \n Azul:@RicardoAnayaC \n Violeta:@Mzavalagc \n Azul marino:@JaimeRdzNL')
ggsave(path = getwd(),device = 'png',plot =  plot_menciones,filename = 'plot_menciones.png')
#system(paste0('open ', getwd(), '/plot_menciones.png'))
system('git add .')
system("git commit -m 'Actualiación' ")
system("git push origin master")
}
}