
####################### Excise tax reform analysis ################


### Code exercise 1

# Written by: Kieran Byrne


# Date: 24/06/2022



# task 1: read in the data, a file called cigarettes.parquet, in Dropbox/Rwanda RRA/data/excise/EBM/final/


cigarettes <- read_parquet(paste0(dropbox_data, 'cigarettes.parquet'))


# create:

# 1. a new week variable

# 2. enacted date variable


cigarettes <- cigarettes%>% 
  
  
  mutate(week = floor_date(receiptdate, 'week'),
         
  month = floor_date(receiptdate, 'month')) %>% 
  
  mutate(enact_date = as.Date('2019-09-18')) %>% 
  
  mutate(diff_time = difftime(receiptdate, enact_date,units = 'weeks'))



# check out the code that creates the plot for the price time series

# here we are using log OLS.

# in previous versions of code we moved from OLS models to a poisson model



# Question 1: Is that OK in this case? why?


#Response from Karia: Relying on Poisson model because the data sets only deals 
#with a specific time frame therefore cannot use OLS as the dependent variable 
#will not be time constrained.

# We are using a fixed effect in this specification

# Question 2: What is the fixed effect? Why do we want to use that here?
#Response from Karia: The fixed effect is the item name as this does not change
#over time. It remains constant. Therefore price variation may in part be driven
#by this fixed effect. 

(felm(log(unit_price) ~ as.factor(week)|itemname, cigarettes) %>% 
    
    broom::tidy(conf.int = TRUE) %>% 
    
    mutate(term = as.Date(substr(term, 16,25))) %>% 
    
    filter(term != last(term)) %>% 
    
    ggplot(aes(x = term, y = estimate))+
    
    geom_point()+
    
    geom_line(linetype = 2)+
    
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high ), alpha = .3)+
    
    geom_vline(xintercept = as.Date('2019-09-18'))+
    
    ylab('% price change')+
    
    scale_y_continuous(labels = scales::percent)+
    
    theme_minimal()+
    
    theme(axis.title.x = element_blank())+
    
    geom_label(inherit.aes = F, aes(x = as.Date('2019-08-30'), y = .3, label = '2019 procedure law')))




############### The request from Pierre #######################


# task 2: plot the variability in prices across packets of cigarettes in the sample period

# hint: use geom_density

# hint: no groups here, just one density plot

Graph1 <-  cigarettes %>% 
  
ggplot(aes(x = unit_price, fill = unit_price))+
  
geom_density(alpha = .3, n = 100000) +
  
xlab('Prices, Rwf')+
  
scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
theme_minimal()+
  
theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
ylab('Density')


# what might be the issue here?

# what might we do to address it?

# task 3: apply a method to make the variability in the data more interpretable

# replot the distribution

Graph2 <-  cigarettes %>% 
    
ggplot(aes(x = unit_price, fill = unit_price))+
    
geom_density(alpha = .3, n = 100000) +
    
xlab('Prices, Rwf')+
    
scale_fill_viridis(option = 'B', discrete = T, end = .7)+
    
scale_x_continuous(limits = c(0,200,000))
  
scale_y_continuous(limits = c(0,.8))+
    
theme_minimal()+
    
theme(legend.title = element_blank(), legend.position = c(.8,.9))+
    
ylab('Density')
  

# task 4: create a new variable that indicates whether the sale is made before or after the enactment date

# hint: use a combination of:
# mutate
# ifelse

cigarettes<-cigarettes %>%
mutate(Reform_Period=ifelse(receiptdate<as.Date('2019-09-18'), 'Before Reform','After Reform'))


# task 5: separately plot the price variation before and after the policy changes

# what do we observe?

cigarettes%>%

group_by(Reform_Period)%>%
  
  ggplot(aes(x = week, y =  unit_price))+
  
  geom_density(stat= 'identity', alpha = .4, aes(fill = Reform_Period))+
  
  theme_minimal()+
  
  theme(axis.title.x = element_blank(), legend.position = c(.2,.7), legend.title = element_blank())+
  
  scale_fill_grey(start = .2, end = 1)+
  
  ylab('unit_price,Rwf') + xlab('week')+
  
 scale_y_continuous(limits = c(0,200000))+
  
  scale_x_date(date_labels = "%b %y", date_breaks = '1 weeks', date_minor_breaks = '0.5 weeks')




















