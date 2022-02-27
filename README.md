# projeto



library(tidyquant)
library(dplyr) 
library(PerformanceAnalytics)
library(ggplot2)
library(tidyverse)
library(writexl)
 jbs <- c('JBSS3.SA') 
 jbs <- tq_get(jbs,  from = '2015-08-05',
                    to = Sys.Date(),
                    get = 'stock.prices') %>%
     group_by(symbol)

  Ri <- jbs%>%
     
     tq_transmute(select = adjusted, 
                           mutate_fun = periodReturn, 
                           period     = 'daily', 
                           col_rename = 'Ri')
 b3<- c('B3SA3.SA') 
 b3 <- tq_get(b3, from = '2015-08-05',
                  to = Sys.Date(),
                  get = 'stock.prices') %>%
  group_by(symbol)
 
     Rm <- b3%>%
       
       tq_transmute(select = adjusted, 
                             mutate_fun = periodReturn, 
                             period     = 'daily', 
                             col_rename = 'Rm')
    capm <- left_join(Ri, Rm, by = c('date' = 'date'))
    write_xlsx(capm, 'dados gerais projeto verao.xlsx')
     ####################################################
     #importando a tabela de noticias

      CAR<-function( x , evento) {
        car_int=seq(-15,15)
        
        for (j in 1:length(evento)){
          for(i in 1:nrow(x)){
            if(x$date[i]==evento[j]){
              reg<-lm(x$Ri[(i-15):(i+15)]~x$Rm[(i-15):(i+15)])
              
              car_int<-cbind(car_int, x[(i-15):(i+15),],residuals(reg))
              
            }
            
          }
        }     
        CAR<-car_int
        
        return(CAR)
        
         }
       
      beta<-function(x, evento) {
        
        beta<-c()
        for (j in 1:length(evento)){
          for(i in 1:nrow(x)){
            if(x$date[i]==evento[j]){
              reg<-lm(x$Ri[(i-15):(i+15)]~x$Rm[(i-15):(i+15)])
              beta<-cbind(beta,coefficients(reg) )
              
              
            }
            
          }
        }     
        
        
        return(beta)
        
      }
      
  
      data<-c('2016-07-04','2017-03-17','2017-05-17',
               '2017-09-13','2020-07-27','2020-10-14',
               '2021-02-25','2022-01-05')

        car = CAR(capm,data)
    interceptos <-as.data.frame(beta(capm , data)) 

    colnames(interceptos)<-c('evento 1','evento 2','evento 3',
                          'evento 4','evento 5','evento 6',
                          'evento 7','evento 8')
    row.names(interceptos)<-c('alfa','beta')
   
     write_xlsx(interceptos, 'interceptos projeto verão.xlsx')
    
     form_table<-function(x,eventos){
   y<-c()
   for(i in 1:31){
     z<-c()
     for(j in 1:eventos){
       z<-cbind(z,x[i,1 + 6*j])
     }
     t<-mean(z)/sd(z)
     k<-cbind(sd(z), mean(z),t)
     y<-rbind(y,k)
     
   }
   y<-cbind(x[,1],y)
   return(y)
   
    }   
eventos=8

estimativa=form_table(car,eventos)
colnames(estimativa)<-c('tempo','desvio padrao', 
                        'retorno_anormal_acumulado_medio',
                        ' estatistica t')
estimativa<-as.data.frame(estimativa)
write_xlsx(estimativa, 'estimativa de estatisticas verao.xlsx')
ggplot(estimativa, 
       aes(x=tempo, y = retorno_anormal_acumulado_medio)) +  
  geom_line(color= 'red')+
  labs(title = "retorno médio por dia", y = 'dia', 
       x='retorno médio')
ggsave("retorno médio por dia.png")
###########################################
#sem joeslay day
data<-c('2016-07-04','2017-03-17',
        '2017-09-13','2020-07-27','2020-10-14',
        '2021-02-25','2022-01-05')

car_sjd = CAR(capm,data)

eventos2=7

estimativa_sjd=form_table(car_sjd,eventos2)

colnames(estimativa_sjd)<-colnames(estimativa)

estimativa_sjd<-as.data.frame(estimativa_sjd)
write_xlsx(estimativa_sjd, ' estimativa de estatisticas sem joeslay day verao.xlsx')
ggplot(estimativa_sjd, 
       aes(x=tempo, y = retorno_anormal_acumulado_medio)) +  
  geom_line(color= 'red')+
  labs(title = "Retorno médio por dia sem Joeslay day", 
       y = 'dia', x='retorno médio')

ggsave("Retorno médio por dia sem Joeslay day.png")
