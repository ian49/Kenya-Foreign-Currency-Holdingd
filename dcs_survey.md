```r
library(tidyverse)
library(readxl)

dcs_survey<- read_csv('foreign_assets.csv')
```

```r
pct_change <- function(x){
  pct=((x-lag(x))/abs(lag(x)))*100
  return(pct)
}

dcs_survey<- dcs_survey %>% 
  unite(Date,Year,Month,sep = '-') %>% 
  mutate(Date=as_date(paste0(Date,'-28')),
         across(matches('nfa'),.fns = ~./120)) 

nfa_change <- dcs_survey %>% 
  mutate_at(vars(matches('nfa')),
            .funs = list(pct=pct_change)) %>% 
  select(-(2:4)) %>% 
  pivot_longer(cols = -Date,names_to = 'var',values_to = 'value')

dcs_survey_long<- dcs_survey%>% 
  pivot_longer(cols = -Date,names_to = 'var',values_to = 'value') %>% 
  mutate(colors=case_when(var=='nfa_nbfi'~'#048A18FC',
                          var=='nfa_cbk_gov'~'#118AB2',
                          TRUE~'#910E07')) 

end_data=tibble(Date=as_date(c('2014-06-28','2016-2-28','2013-10-28')),
                 value=c(0,3100,5400),
                 label=c('Non-bank Financial Institutions','Net Foreign Assets',
                         'Central Bank/Government'),
                 colors=c('#048A18FC','#910E07','#118AB2'))

end_abs_data<- dcs_survey_long %>% 
  filter(Date==max(Date)) %>% 
  mutate(label=paste0('$',round(value*.001,1),'b'))

end_pct_label<- nfa_change %>% 
  filter(Date==max(Date),str_detect(var,'pct')) %>% 
  mutate(colors=c('#910E07','#118AB2','#048A18FC'),
         label=round(value,1),
         value=c(3450,5000,-1900))
         
dcs_survey_long%>% 
  ggplot(aes(Date,value,group=var)) +
  geom_line(aes(color=colors),size=1.5,show.legend = F)+
  geom_text(data = end_data,aes(Date,value,label=label,color=colors),
            show.legend = F,inherit.aes = F,size=5)+
  geom_text(data = end_abs_data,
            aes(Date+months(3),value,label=label),size=5)+
  geom_text(data = end_pct_label,
            aes(Date+months(3),value,label=paste0('(',label,'%)'),
                color=colors),size=5,show.legend = F)+
  scale_y_continuous(labels = scales::label_dollar(accuracy = .1,scale = .001,
                                                   suffix = 'b'))+
  scale_color_manual(values=text_data$colors)+
  labs(x='',y='',
       title = 'Composition of foreign currency holdings',
       subtitle = 'Monthly data to July,2022',
       caption = 'Source: Central Bank of Kenya. Chart and calculations by @mutwiriian')+
  theme_minimal()+
  theme(
    axis.text = element_text(size=12),
    plot.caption = element_text(size=12,hjust = -.05,vjust = 2),
    plot.title = element_text(face = 'bold',colour = 'grey20'),
    plot.subtitle = element_text(face = 'bold',color = 'grey50'),
    plot.title.position = 'panel'
  )
```

```r
ggsave('foreign_assets.png',dpi=300,width = 4000,height = 2500,units = 'px')
```

