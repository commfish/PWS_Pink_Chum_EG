#Analysis by Rich Brenner 
#Updated 6 Sept. 2017

# Notes ----
# area-under-the-curve (AUC) data for individual streams, not
# adjusted for (divided by) stream life.
# Data are from the tab 'AUC_Pivot_Index' from the Spreadsheet titled:
#   'PWS_AUC_1962-2016_Subset for 2015 index streams.xls' made by
# Steve Moffitt (recently retired ADF&G, Cordova). 

# These data have been subsetted to only include the ~134 streams
# determined in 2015 to be used for future surveys. There were ~215 streams
# originally surveyed between 1963-2014, but Steve did not include all of these   here.
# In the future: recreate district-wide adjusted-AUC estimates from the original raw data to make this exercise reproducible! To do this:

# load----
library(arm)
library(tidyverse)
options(scipen=9999)
theme_set(theme_bw(base_size=12)+ 
			 	theme(panel.grid.major = element_blank(),
			 			panel.grid.minor = element_blank()))

#data----
pws_pink_chum <- read_csv("data/PWS_Pink_Chum_EG.csv")

#adjust area-under-the-curve values for stream life
pws_pink_chum %>%  
  mutate(pink_adjstd = round(Sum_P_AUC/pink_strm_life, digits =0),
         chum_adjstd = round(Sum_C_AUC/chum_strm_life, digits=0),
  		 dist_name = case_when(district=="221" ~ "Eastern",
  		 							 district=="222" ~ "Northern",
  		 							 district=="223" ~ "Coghill",
  		 							 district=="224" ~ "Northeastern",
  		 							 district=="225" ~ "Eshamy",
  		 							 district=="226" ~ "Southwestern",
  		 							 district=="227" ~ "Monthague",
  		 							 district=="228" ~ "Southeastern",
  		 							 district=="229" ~ "Unakwik"),
  		 dist_num_name = paste0(district,":", dist_name)) -> pws_pink_chum

# Pink ----
# For pink salmon analyses, combine districts 222 and 229 by renaming 229 as 222.  
pws_pink_chum %>%  
  	mutate(district = ifelse(district == 229, 222, district)) %>% 
  	group_by(year, broodline, district) %>% 
  	filter(broodline == "Even") %>%
  	summarize(pink_dist = sum(pink_adjstd), n = n()) -> pink_even
   
pws_pink_chum %>%  
  	mutate(district = ifelse(district == 229, 222, district)) %>% 
  	group_by(year, broodline, district) %>% 
  	filter(broodline == "Odd")%>%
  	summarize(pink_dist = sum(pink_adjstd), n = n()) -> pink_odd                                                              

# pink quantiles ----
# calculate 20th and 60th percentiles for EVEN year pink salmon for each district
# for proposed spawning escapement goals

pink_even %>%
  filter(year >"1980") %>%  #Only includes years from 1982-present
  group_by(district) %>%  
  summarize(p = list(c(0.20, 0.60)), q = list(quantile(pink_dist, probs))) %>%
  unnest() -> pink_even_quantiles

# calculate 25th and 75th percentiles for ODD year pink salmon for each district
# for proposed spawning escapement goals

pink_odd %>%
  filter(year >"1979") %>%  #Only includes years from 1981-present, but not 2016
  group_by(district) %>%  
  summarize(p = list(c(0.25, 0.75)), q = list(quantile(pink_dist, probs))) %>%
  unnest() -> odd_pink_quantiles

#upper quantiles....use this for the figures
up_pink_odd <- odd_pink_quantiles %>%
  filter(p == .75)

low_pink_odd <- odd_pink_quantiles %>%
  filter(p== .25)


# Figure of PINK salmon escapments and proposed goals
# Proposed goal for EVEN year pink salmon

pink_even_quantiles


pink_even %>% 
	ggplot(aes(year, pink_dist)) + 
	geom_point() +
	labs(x = "Years", y = "Escapement") +
	geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005) + #shade years w/ too few surveys
	geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005) + #shade years w/ too few surveys
	geom_hline(data=filter(pink_even_quantiles, p==0.75), aes(yintercept = q)) + #add upper line for  escapement goal
	geom_hline(data=low_pink_even, aes(yintercept = q))+#add lower line for escapement goal
	facet_wrap(~ district, labeller = label_both, ncol = 2, scales = "free_y")
p_even


#Proposed goal for ODD year pink salmon
p_odd <- ggplot(data = district_pink_odd) +
	geom_point(mapping = aes(x = year, y = pink_dist)) +
	labs(x = "Years", y = "Escapement") +
	geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005) + #shade years w/ too few surveys
	geom_hline(data=up_pink_odd, aes(yintercept = q))+ #add upper line for  escapement goal
	geom_hline(data=low_pink_odd, aes(yintercept = q))+#add lower line for escapement goal
	facet_wrap(~ district, ncol = 2, scales = "free_y")
p_odd


# Chum ----
# Summarize chum harvests across districts for each year, we can drop broodlines
pws_pink_chum %>%  
  filter(district != "225", district !="226", district !="227", # Only districts of interest
         district != "229") %>%
  group_by(year, district, dist_num_name) %>%
  summarize(chum_dist = sum(chum_adjstd, na.rm = TRUE),
            n = n()) -> chum_sum



# quantiles ----
# calculate 20th and 60th percentiles for chum salmon for each chum district
# for proposed spawning escapement goals
probs <- c(0.20, 0.60)
chum_sum %>%
  filter(year >"1979", year != "2016") %>%  #Only includes years from 1980-present, but not 2016
  group_by(district, dist_num_name) %>%  
  summarize(p = list(probs), q = list(quantile(chum_dist, probs))) %>%
  unnest() -> chum_quantiles

#upper and lower quantiles for individual districts....for geom_hlines....maybe
#upper quantiles
upper_chum <- chum_quantiles %>%
  filter(p == .6)
upper_chum

lower_chum <- chum_quantiles %>%
  filter(p== .2)
lower_chum


# Figures ----
#Figure of CHUM salmon escapements and proposed goals
ggplot (data = district_chum_sum) +
  geom_point(mapping = aes(x = year, y = chum_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_hline(data=upper_chum, aes(yintercept = q))+ #add upper line for  escapement goal
  geom_hline(data=lower_chum, aes(yintercept = q))+ #add lower line for escapement goal
  facet_wrap(~ district, labeller= label_both, ncol=2, scales = "free_y")+
ggsave("figures/C.png", dpi=400, width=8, height=5, units='in')

#Figure of PINK salmon escapments and proposed goals
#Proposed goal for EVEN year pink salmon
p_even <- ggplot (data = district_pink_even) +
  geom_point(mapping = aes(x = year, y = pink_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1964, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_hline(data=up_pink_even, aes(yintercept = q))+ #add upper line for  escapement goal
  geom_hline(data=low_pink_even, aes(yintercept = q))+#add lower line for escapement goal
  facet_wrap(~ district, labeller = label_both, ncol = 2, scales = "free_y")
p_even


#Proposed goal for ODD year pink salmon
p_odd <- ggplot(data = district_pink_odd) +
  geom_point(mapping = aes(x = year, y = pink_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005) + #shade years w/ too few surveys
  geom_hline(data=up_pink_odd, aes(yintercept = q))+ #add upper line for  escapement goal
  geom_hline(data=low_pink_odd, aes(yintercept = q))+#add lower line for escapement goal
  facet_wrap(~ district, ncol = 2, scales = "free_y")
p_odd


##############################################################
#PRead Harvest date obtained from the 2017 PWS wild chum salmon forecast spreadsheet
c_harvest<- read_csv("data/Chum_Harvest_Rate.csv")
c_harvest

#Create variables for possible maximum and minimum harvests
#adjusted_AUC is from the ~214 area index streams, not the reduced stream count of 134 streams
#maximum harvest estimate from area-under-the-curve (AUC) adjusted for stream life.
#minimum harvest estimate expands adjusted_AUC escapement estimate to account for observer
#efficiency of 0.436 and the proportion of overall escapement represented by the ~214 streams
#surveyed (0.80). Estimates are from Fried et al., Fish and Shellfish Study I (EVOS)
#and based on pink salmon!
c_harvest %>% 
  mutate(max_harv = Harvest/(Harvest+adjusted_AUC)*100,
  		 min_harv = Harvest/((adjusted_AUC/0.436/.80)+Harvest)*100,
  		 min_run = Harvest + adjusted_AUC,
  		 max_run = Harvest + (adjusted_AUC/0.436/0.80)) -> c_harvest

#Plot of maximum and minimum harvest rates for PWS wild chum salmon
chum_perc_harv <- ggplot(data = c_harvest)+
  geom_line(mapping = aes(Year, max_harv))+
  geom_line(mapping = aes(Year,  min_harv))+
  labs(x = "Years", y = "% Harvest")
chum_perc_harv

#Estimated number of wild chum harvested in PWS
chum_harv <- ggplot(data = c_harvest)+
  geom_line(mapping = aes(x = Year, y = Harvest))+
  labs(x = "Years", y = "Harvest")
chum_harv

chum_total_max <- ggplot(data = c_harvest)+
  geom_line(mapping = aes(x = Year, y = max_run))+
  labs(x = "Years", y = "Total Run Size")
chum_total_max

chum_total_min <- ggplot(data = c_harvest)+
  geom_line(mapping = aes(x = Year, y = min_run))+
  labs(x = "Years", y = "Total Run Size")
chum_total_min


#Wrap the 3 figures above into a singe figure 1 column???
facet_wrap(c(chum_))






