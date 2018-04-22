rm(list=ls())
setwd('~')
wd<-list.dirs(path = 'Documents', full.names = T)[grep('debate', list.dirs(path = 'Documents', full.names = T))][1]
setwd(wd)
tokens<-read.csv('./source/tokens.csv', stringsAsFactors = F)





library(rtweet)
library(data.table)
library(ggplot2)
library(tm)

procesa_corpus<-function(obj){
  corpus<-Corpus(VectorSource(obj))
  corpus <- tm_map(corpus, removeWords, c('lopezobrador_',
                                          'JoseAMeadeK',
                                          'JaimeRdzNL',
                                          'RicardoAnayaC',
                                          'Mzavalagc'))
  
  
  corpus<-tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, PlainTextDocument)
  
  
  
  corpus <- tm_map(corpus, removeWords, stopwords(kind='es'))
  
  
  
  #corpus<-tm_map(corpus, stemDocument)
  
  m<-as.matrix(TermDocumentMatrix(corpus))
  v<-sort(rowSums(m),decreasing=TRUE)
  corpus <- data.frame(word = names(v),freq=v)
  return(corpus)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


Sys.setenv(TZ='America/Mexico_City')
token_1<-create_token(app = tokens$app[1],
                    consumer_key = tokens$consumer_key[1], 
                    consumer_secret = tokens$consumer_secret[1])


data<-NULL
data_cons<-data.table(hora=.POSIXct(Sys.time(), 'America/Mexico_City'), n_tweets=NA, n_amlo=NA,
                      n_meade=NA, n_anaya=NA, n_bronco=NA, n_maza=NA)
data_cons_nort<-data.table(hora=.POSIXct(Sys.time(), 'America/Mexico_City'), n_tweets=NA, n_amlo=NA,
                      n_meade=NA, n_anaya=NA, n_bronco=NA, n_maza=NA)

data_i<-data.table(hora=NA, n_tweets=NA, n_amlo=NA,
                      n_meade=NA, n_anaya=NA, n_bronco=NA, n_maza=NA)
data_i_nort<-data.table(hora=NA, n_tweets=NA, n_amlo=NA,
                   n_meade=NA, n_anaya=NA, n_bronco=NA, n_maza=NA)

pals_5min<-NULL

actualiza_git<-T
horas<-4


#1:(60*horas)
for(i in 1:5){
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
data_i$n_amlo<-sum(unlist(lapply(data$mentions_screen_name, function(x) sum(grepl('lopezobrador_', x)))))
data_i$n_meade<-sum(unlist(lapply(data$mentions_screen_name, function(x) sum(grepl('JoseAMeadeK', x)))))
data_i$n_anaya<-sum(unlist(lapply(data$mentions_screen_name, function(x) sum(grepl('RicardoAnayaC', x)))))
data_i$n_maza<-sum(unlist(lapply(data$mentions_screen_name, function(x) sum(grepl('Mzavalagc', x)))))
data_i$n_bronco<-sum(unlist(lapply(data$mentions_screen_name, function(x) sum(grepl("JaimeRdzNL", x)))))
data_cons<-rbind(data_cons, data_i)

#####NO RTS#####
data_nort<-data[data$is_retweet==T,]

data_i_nort$hora<-.POSIXct(Sys.time(), 'America/Mexico_City')
data_i_nort$n_tweets<-nrow(data_nort)
data_i_nort$n_amlo<-sum(unlist(lapply(data_nort$mentions_screen_name, function(x) sum(grepl('lopezobrador_', x)))))
data_i_nort$n_meade<-sum(unlist(lapply(data_nort$mentions_screen_name, function(x) sum(grepl('JoseAMeadeK', x)))))
data_i_nort$n_anaya<-sum(unlist(lapply(data_nort$mentions_screen_name, function(x) sum(grepl('RicardoAnayaC', x)))))
data_i_nort$n_maza<-sum(unlist(lapply(data_nort$mentions_screen_name, function(x) sum(grepl('Mzavalagc', x)))))
data_i_nort$n_bronco<-sum(unlist(lapply(data_nort$mentions_screen_name, function(x) sum(grepl("JaimeRdzNL", x)))))
data_cons_nort<-rbind(data_cons_nort, data_i_nort)

pals_5min<-c(data_nort$text, pals_5min)



#############plotea menciones cadaa 5 minutos#########
######Grafica con rt########
if (i %%5 ==0){
  pal_amlo<-paste(pals_5min[grepl('lopezobrador_', pals_5min )], collapse = ' ')
  pal_meade<-paste(pals_5min[grepl('JoseAMeadeK', pals_5min)], collapse = ' ')
  pal_bronco<-paste(pals_5min[grepl('JaimeRdzNL', pals_5min)], collapse = ' ')
  pal_anaya<-paste(pals_5min[grepl('RicardoAnayaC', pals_5min)], collapse = ' ')
  pal_maza<-paste(pals_5min[grepl('Mzavalagc', pals_5min)], collapse = ' ')

  pal<-list(pal_amlo, pal_meade, pal_anaya, pal_bronco, pal_maza )
  
  
  pal<-lapply(pal, procesa_corpus)
  names(pal)<-c('amlo', 'meade','anaya', 'bronco', 'margarita')
  
  pals_5min<-NULL
  plots_wd<-list()
  for (a in 1:length(pal)){
  pal[[a]]$word<-factor(pal[[a]]$word,
                        levels = pal[[a]]$word[order(pal[[a]]$freq,
                                                     decreasing = F)])
  plots_wd[[a]]<-ggplot(data=pal[[a]][pal[[a]]$freq>quantile(pal[[a]]$freq, 0.97),], aes(x=word, y=freq))+
    geom_bar(stat = 'identity')+
    coord_flip()+
    theme_light()+
    xlab('')+
    ylab('')+
    ggtitle(paste('Corte de las ', Sys.time() ))
  }
ggsave( filename = 'palabras_5mn.png',device = 'png',plot = 
            do.call(multiplot, c(plots_wd, cols=2)))  
  
  message('actuaizando gráficas de 5 minutos...')
  
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
  ggtitle('Número de menciones por minuto',
          subtitle = paste0('Gris:Número de menciones total \n',
                            'Marrón:@lopezobrador_ \n',
                            'Rojo:@JoseAMeadeK \n',
                            'Azul:@RicardoAnayaC \n',
                            'Violeta:@Mzavalagc \n',
                            'Azul marino:@JaimeRdzNL \n',
                            'Nota: Las menciones no son excluyentes entre candidatos.\n',
                            'También se consideran tweets que citan menciones a los candidatos para el total de tweets'))
ggsave(path = getwd(),device = 'png',plot =  plot_menciones,filename = 'plot_menciones.png')
#system(paste0('open ', getwd(), '/plot_menciones.png'))


######Grafica SIN rt########

plot_menciones_nort<-ggplot(data=data_cons_nort[-1,], aes(x=hora, y=n_tweets))+
  geom_line(color='grey')+
  geom_line(aes(x=hora, y=n_amlo), color='#AC2C2C')+
  geom_line(aes(x=hora, y=n_meade), color='red')+
  geom_line(aes(x=hora, y=n_anaya), color='blue')+
  geom_line(aes(x=hora, y=n_bronco), color='#2D3672')+
  geom_line(aes(x=hora, y=n_maza), color='#B917BF')+
  theme_light()+
  ylab('Número de menciones')+
  xlab('Hora')+
  ggtitle('Número de menciones por minuto (menos RTS)',
          subtitle = paste0('Gris:Número de menciones sin Retweets \n',
                            'Marrón:@lopezobrador_ \n',
                            'Rojo:@JoseAMeadeK \n',
                            'Azul:@RicardoAnayaC \n',
                            'Violeta:@Mzavalagc \n',
                            'Azul marino:@JaimeRdzNL \n',
                            'Nota: Las menciones no son excluyentes entre candidatos.\n',
                            'También se consideran tweets que citan menciones a los candidatos para el total de tweets'))
ggsave(path = getwd(),device = 'png',plot =  plot_menciones_nort,filename = 'plot_menciones_nort.png')
#system(paste0('open ', getwd(), '/plot_menciones.png'))


data_cons_cumsum<-cbind.data.frame(hora=data_cons_nort$hora[-1],
                                   apply(data_cons_nort[-1, c( 'n_amlo', 'n_meade',
                                                          'n_anaya', 'n_bronco', 'n_maza')], 2,  cumsum ))

data_cons_cumsum[, c('n_amlo',
                     'n_meade',
                     'n_anaya',
                     'n_bronco',
                     'n_maza')]<-round(data_cons_cumsum[, c('n_amlo',
                                                      'n_meade',
                                                      'n_anaya',
                                                      'n_bronco',
                                                      'n_maza')]/rowSums(data_cons_cumsum[, c('n_amlo',
                                                                                              'n_meade','n_anaya',
                                                                                              'n_bronco',
                                                                                              'n_maza')])*100,1)

######Grafica PORC MECIONES ACUMULADA########

plot_menciones_nort_porc<-ggplot(data=data_cons_cumsum, aes(x=hora, y=n_amlo))+
  geom_line(color='#AC2C2C')+
 # geom_line(aes(x=hora, y=n_amlo), color='#AC2C2C')+
  geom_line(aes(x=hora, y=n_meade), color='red')+
  geom_line(aes(x=hora, y=n_anaya), color='blue')+
  geom_line(aes(x=hora, y=n_bronco), color='#2D3672')+
  geom_line(aes(x=hora, y=n_maza), color='#B917BF')+
  theme_light()+
  ylab('% acumulado de menciones')+
  xlab('Hora')+
  ylim(c(0, 100))+
  ggtitle('Procentaje acumulado de menciones sin Retweets',
          subtitle = paste0('Marrón:@lopezobrador_ \n',
                            'Rojo:@JoseAMeadeK \n',
                            'Azul:@RicardoAnayaC \n',
                            'Violeta:@Mzavalagc \n',
                            'Azul marino:@JaimeRdzNL \n'))
ggsave(path = getwd(),
       device = 'png',
       plot =  plot_menciones_nort_porc,
       filename = 'plot_menciones_nort_porc.png')

if (actualiza_git==T){
system('git add .')
system("git commit -m 'Actualiación' ")
system("git push origin master")
}

}
}