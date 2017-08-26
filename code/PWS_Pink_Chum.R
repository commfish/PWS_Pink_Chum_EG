#Analysis by Rich Brenner 
#Updated 21 Aug. 2017

#load----
library(arm)
library(gdata)
library(tidyverse)
options(scipen=9999)

#data----
PWSPinkChum <- read_csv("data/PWS_Pink_Chum_EG.csv")

PWSPinkChum %>%  #adjust area under the curve values for stream life
  mutate(pink_adjstd = round(Sum_P_AUC/pink_strm_life, digits =0),
         chum_adjstd = round(Sum_C_AUC/chum_strm_life, digits=0)) -> PWSPinkChum
glimpse(PWSPinkChum)


#For pink salmon analyses, combine districts 222 and 229 by renaming 229 as 222.  
  PWSPinkChum %>%   
  mutate(district = ifelse(district == 229, 222, district)) -> out #out is a temporary file for
                                                                  #pink analyses only!

  
#Summarize even-year pink stocks by district. Note that 229 and 222 have already been combined
district_pink_even <- out %>% 
  group_by(year, broodline, district) %>% 
  filter(broodline == "Even")%>%
  summarize(pink_dist = sum(pink_adjstd), #sums each yearxbroodlinexdistrict combination
            n = n() #counts the number of streams surveyed per district
  )  
glimpse(district_pink_odd)


#Summarize odd-year pink stocks by district. Note that 229 and 222 have already been combined
district_pink_odd <- out %>% 
  group_by(year, broodline, district) %>% 
  filter(broodline == "Odd")%>%
  summarize(pink_dist = sum(pink_adjstd),
            n = n() #counts the number of streams surveyed per district
)
glimpse(district_pink_odd)




######################################################################################
#calculate 20th and 60th percentiles for EVEN year pink salmon for each district
probs <- c(0.20, 0.60)
even_pink_quantiles <- district_pink_even %>%
  filter(year >"1980") %>%  #Only includes years from 1982-present
  group_by(district) %>%  
  summarize(p = list(probs), q = list(quantile(pink_dist, probs))) %>%
  unnest()
glimpse(even_pink_quantiles)
even_pink_quantiles




#######################################################################################
#calculate 20th and 60th percentiles for ODD year pink salmon for each district
probs <- c(0.25, 0.75)
odd_pink_quantiles <- district_pink_odd %>%
  filter(year >"1979") %>%  #Only includes years from 1981-present, but not 2016
  group_by(district) %>%  
  summarize(p = list(probs), q = list(quantile(pink_dist, probs))) %>%
  unnest()
glimpse(odd_pink_quantiles)
odd_pink_quantiles



########################################################################################
#Summarize CHUM harvests across districts for each year, we can drop broodlines
district_chum_sum <- PWSPinkChum %>%  
  filter(district != "225", district !="226", district !="227", # Only districts of interest
         district != "229")%>%
  group_by(year, district) %>%
  summarize(chum_dist = sum(chum_adjstd),
            n = n()#counts the number of streams per district
) 
glimpse(district_chum_sum)


#######################################################################################
#calculate 20th and 60th percentiles for chum salmon for each chum district
probs <- c(0.20, 0.60)
chum_quantiles <- district_chum_sum %>%
  filter(year >"1979", year != "2016") %>%  #Only includes years from 1980-present, but not 2016
  group_by(district) %>%  
  summarize(p = list(probs), q = list(quantile(chum_dist, probs))) %>%
  unnest()
glimpse(chum_quantiles)
chum_quantiles

#upper and lower quantiles for individual districts....for geom_hlines....maybe
#upper quantiles
upper_chum <- chum_quantiles %>%
  filter(p == .6)

lower_chum <- chum_quantiles %>%
  filter(p== .2)


######################################################################################

#FIGURES
#Figure of CHUM salmon escapements and proposed goals
c <- ggplot (data = district_chum_sum) +
  theme_bw() +
  geom_point(mapping = aes(x = year, y = chum_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_hline(y-intercept = 100000)+
  facet_wrap(~ district, nrow = 4, scales = "free_y")
c



#Figure of PINK salmon escapments and proposed goals
p_even <- ggplot (data = district_pink_even) +
  theme_bw()+
  geom_point(mapping = aes(x = year, y = pink_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  facet_wrap(~ district, nrow = 4, scales = "free_y")
p_even


p_odd <- ggplot (data = district_pink_odd) +
  theme_bw()+
  geom_point(mapping = aes(x = year, y = pink_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  facet_wrap(~ district, nrow = 4, scales = "free_y")
p_odd



  



