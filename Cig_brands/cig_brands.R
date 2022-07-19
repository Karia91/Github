


########### Exercise data cleaning & branding ###########


# date: 11/07/2022

# author: Kieran Byrne




# 1. read in the cigarette data

cigarettes <- read_parquet(paste0(dropbox_data, 'cigarettes.parquet'))



# 2. Use the itemname variable to verify if all of the items in the data are actually cigarettes

#KS: Counted the number of brands.
#KS: Created a dataframe that I could use as a reference point going forward.

brandtype <- cigarettes %>% group_by(itemname) 
itemizedbrandtype <- summarize(brandtype, count = n())

rm("itemizebrandtype", "brandtype")


# 3. Use the itemname to group cigarettes by brand 

# hint 1: use grepl() to identify each brand

# hint 2: use case_when for multiple if statements

cigarettes <- cigarettes %>% mutate(brand = case_when(grepl('DUNHILL', 'TOB', 'INTORE',
toupper(itemname)) -'DUNHILL', 'TOB', 'INTORE'))


# an example:

# cigarrettes <- cigarrettes %>% 
#    
# mutate(brand = case_when(grepl('DUNHILL', toupper(itemname))~'DUNHILL',
#                             ))


#change the data frame here to DUNHILL so that cigarettes can still be used later on


#Dunhill

Dunhill<-cigarettes%>% mutate(brand=case_when(grepl('DUNHILL', toupper(itemname))~'DUNHILL'))%>% filter(!is.na(brand))

Dunhil<-cigarettes%>% mutate(brand=case_when(grepl('DUNHIL\ BLUE', toupper(itemname))~'DUNHIL'))%>% filter(!is.na(brand))

Dunhill <- rbind(Dunhill, Dunhil)

rm("Dunhil")


#Impala

Impala<-cigarettes%>% mutate(brand=case_when(grepl('IMPALA', toupper(itemname))~'Impala'))%>% filter(!is.na(brand))


#Intore

Intore<-cigarettes%>% mutate(brand=case_when(grepl('INTORE', toupper(itemname))~'Intore'))%>% filter(!is.na(brand))


#Pal_Mall

Pall_Mall<-cigarettes%>%mutate(brand=case_when(grepl('PALL\ MALL', toupper(itemname))~'PALL_MALL'))%>% filter(!is.na(brand))


#Sweet_Menthol: (Made the assumption that Sweet Menthol and SM are the same brand)

Sweet_Menthol<-cigarettes%>%mutate(brand=case_when(grepl('SWEET\ MENTHOL', toupper(itemname))~'SWEET_MENTHOL'))%>%filter(!is.na(brand))

SM<-cigarettes%>% mutate(brand=case_when(grepl('SM', toupper(itemname))~'SM'))%>% filter(!is.na(brand))

Sweet_Menthol<-rbind(SM, Sweet_Menthol)

rm("SM")


#African Tobacco
African_Tobacco<-cigarettes%>%mutate(brand=case_when(grepl('AFRICAN\ TOBACCO', toupper(itemname))~'AFRICAN_TOBACCO'))%>%filter(!is.na(brand))

#CK
CK<-cigarettes%>%mutate(brand=case_when(grepl('CK', toupper(itemname))~'CK'))%>%filter(!is.na(brand))

#Malawi_Sugar
Malawi_Sugar<-cigarettes%>%mutate(brand=case_when(grepl('MALAWI', toupper(itemname))~'Malawi_sugar'))%>%filter(!is.na(brand))




# 4. check if there are any outliers for price 

# hint: use ggplot and geom_density to do this


#African_Tobacco

ATDensity <-  African_Tobacco %>% 
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

#Quite a few outliers for African Tobacco


#CK

CKDensity<- CK %>% 
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  #not clear that there are too many outliers for CK


#Dunhill

DunhillDensity<-Dunhill%>%
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  #Not clear that there are many outliers for Dunhill either


#Impala

ImpalaDensity<-Impala%>%
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  #not clear that there are many outliers


#Intore

IntoreDensity<-Intore%>%
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  #Many outliers for Intore as well


#Malawi Density
MalawiDensity <- Malawi_Sugar%>%
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  #Some outliers for Malawi Sugar 


#Pal_Mall

Pal_MallDensity<- Pall_Mall%>%
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')


#Sweet_Menthol

Sweet_MentholDensity <- Sweet_Menthol%>%
  
  ggplot(aes(x = unit_price, fill = unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T, end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  #Not many outliers for Sweet_Menthol 


# 5. handle the outliers

# question: what's the best way to do this?

#Karia: I used Z-Scores as the guiding tool and removed values with z scores outside of 2sd from the mean

#African Tobacco
African_Tobacco$Z_scores<-(African_Tobacco$unit_price-mean(African_Tobacco$unit_price))/sd(African_Tobacco$unit_price)

#CK
CK$Z_scores<-(CK$unit_price-mean(CK$unit_price))/sd(CK$unit_price)

#Dunhill
Dunhill$Z_scores<-(Dunhill$unit_price-mean(Dunhill$unit_price))/sd(Dunhill$unit_price)

#Impala
Impala$Z_scores<-(Impala$unit_price-mean(Impala$unit_price))/sd(Impala$unit_price)

#Intore
Intore$Z_scores<-(Intore$unit_price-mean(Intore$unit_price))/sd(Intore$unit_price)

#Malawi_Sugar
Malawi_Sugar$Z_scores<-(Malawi_Sugar$unit_price-mean(Malawi_Sugar$unit_price))/sd(Malawi_Sugar$unit_price)

#Pall_Mall
Pall_Mall$Z_scores<-(Pall_Mall$unit_price-mean(Pall_Mall$unit_price))/sd(Pall_Mall$unit_price)

#Sweet_Menthol
Sweet_Menthol$Z_scores<-(Sweet_Menthol$unit_price-mean(Sweet_Menthol$unit_price))/sd(Sweet_Menthol$unit_price)



# 6. Plot the price distribution of each of the brands using ggplot

#Karia: i filtered out Z_scores 2 standard deviations away 


#African Tobacco

FilteredAT<- filter(African_Tobacco, Z_scores>-2 & Z_scores<2 )

  FilteredATDensity<-FilteredAT%>%
  
  ggplot(aes(x = unit_price, fill=unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')
  

#CK  
FilteredCK<- filter(CK, Z_scores>-2 & Z_scores<2)

FilteredCKDensity<-FilteredCK%>%
  
  ggplot(aes(x = unit_price, fill=unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
  
  scale_x_continuous(limits = c(0,10000))

  scale_y_continuous(limits = c(0,(.08)))+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

    
#Dunhill
FilteredDunhill<-filter(Dunhill, Z_scores>-2 & Z_scores<2)

FilteredDunhillDensity<-FilteredDunhill%>%

  ggplot(aes(x = unit_price, fill=unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
  
  scale_x_continuous(limits = c(0,100000))+

  scale_y_continuous(limits = c(0,.08))+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  
#Impala

FilteredImpala<- filter(Impala, Z_scores>-2 & Z_scores<2)

FilteredImpala <- FilteredImpala%>%
  
  ggplot(aes(x = unit_price, fill=unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  #scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
  
  scale_x_continuous(limits = c(0,100000))+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')
  

  
#Intore
  
FilteredIntore<- filter(Intore, Z_scores>-2 & Z_scores<2)
  
FilteredIntoreDensity<-FilteredIntore%>%
  
  ggplot(aes(x = unit_price, fill=unit_price))+
  
  geom_density(alpha = .3, n = 100000) +
  
  xlab('Prices, Rwf')+
  
  scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
  
  scale_x_continuous(limits = c(0,80000))

  scale_y_continuous(limits = c(0,.08))+
  
  theme_minimal()+
  
  theme(legend.title = element_blank(), legend.position = c(.8,.9))+
  
  ylab('Density')

  
#Malawi_Sugar
  
  FilteredMalawi_Sugar<- filter(Malawi_Sugar, Z_scores>-2 & Z_scores<2)
  
  FilteredMalawi_SugarDensity<-FilteredMalawi_Sugar%>%
    
    ggplot(aes(x = unit_price, fill=unit_price))+
    
    geom_density(alpha = .3, n = 10000) +
    
    xlab('Prices, Rwf')+
    
    scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
    
    theme_minimal()+
    
    theme(legend.title = element_blank(), legend.position = c(.8,.9))+
    
    ylab('Density')
  
  
#Pall_Mall
  
  FilteredPall_Mall<- filter(Pall_Mall, Z_scores>-2 & Z_scores<2)
  
  FilteredPall_Mall<-FilteredPall_Mall%>%
    
    ggplot(aes(x = unit_price, fill=unit_price))+
    
    geom_density(alpha = .3, n = 10000) +
    
    xlab('Prices, Rwf')+
    
    scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
    
    theme_minimal()+
    
    theme(legend.title = element_blank(), legend.position = c(.8,.9))+
    
    ylab('Density')
  

#Sweet_Menthol  
  
  FilteredSweet_Menthol<- filter(Sweet_Menthol, Z_scores>-2 & Z_scores<2)
  
  FilteredSweet_Menthol<-FilteredSweet_Menthol%>%
    
    ggplot(aes(x = unit_price, fill=unit_price))+
    
    geom_density(alpha = .3, n = 10000) +
    
    xlab('Prices, Rwf')+
    
    scale_fill_viridis(option = 'B', discrete = T,  end = .7)+
    
    theme_minimal()+
    
    theme(legend.title = element_blank(), legend.position = c(.8,.9))+
    
    ylab('Density')