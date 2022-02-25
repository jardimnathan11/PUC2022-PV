# projeto



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
     RiRm <- left_join(Ri, Rm, by = c('date' = 'date'))
     exp_ri<-lm(RiRm$Ri~RiRm$Rm)
     AR<-residuals(exp_ri)
     capm<-cbind(RiRm, AR)
