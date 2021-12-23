
# guatemala 2014
library(tidyverse)
c <- read.csv("Data/gt_1x1.csv")
m  <- read.csv("Data/gt_women_1x1.csv") 
cs <-c %>% rename(woman_age = mother_age) %>% left_join(m) 

# rates guatem
loc_guat <- fertestr::get_location_code("Guatemala")
gtm_mx <- DemoToolsData::WPP2019_lt %>% 
  filter(LocID == loc_guat %>% as.integer(), Year == 2013, Sex == "b") %>% 
  as.data.frame()
ggplot(gtm_mx,aes(AgeStart,mx))+geom_step()+scale_y_log10()+
  geom_step(aes(AgeStart,mx+.001),col=2)+
  geom_step(aes(AgeStart,mx*1.1),col=3)



# check
cs %>% 
  group_by(woman_age) %>% 
  summarise(c = sum(child_no), m = unique(woman_no)) %>% 
  mutate(c/m) %>% 
  ggplot() + geom_line(aes(woman_age, `c/m`))

# cambio de .0001, .001 y .005 en 15-24 y 0
avg_mx    = gtm_mx %>% filter(AgeStart <30)
impact_change <- cs %>% 
  group_by(woman_age) %>% 
  summarise(
            mean_childs = sum(child_no)/unique(woman_no),
            # 0
            prop_age_0 = sum(child_no[child_age%in%0])/sum(child_no),
            mean_age_0 = .5,
            impact_0_.0001 = .0001 * (prop_age_0 * (mean_age_0*15) + 15 * (1-prop_age_0)) * 100,
            impact_0_.0002 = .0002 * (prop_age_0 * (mean_age_0*15) + 15 * (1-prop_age_0)) * 100,
            impact_0_.003  =  .003 * (prop_age_0 * (mean_age_0*15) + 15 * (1-prop_age_0)) * 100,
            # 15-24
            prop_age_15_24 = sum(child_no[child_age%in%15:24])/sum(child_no),
            mean_age_15_24 = .5 + sum(child_no[child_age%in%15:24]*child_age[child_age%in%15:24])/sum(child_no[child_age%in%15:24]),
            impact_15_24_.0001 = .0001 * (prop_age_15_24 * (mean_age_15_24*15) + 15 * (1-prop_age_15_24)) * 100,
            impact_15_24_.0002 = .0002 * (prop_age_15_24 * (mean_age_15_24*15) + 15 * (1-prop_age_15_24)) * 100,
            impact_15_24_.003  =  .003 * (prop_age_15_24 * (mean_age_15_24*15) + 15 * (1-prop_age_15_24)) * 100) %>% 
            
  mutate(across(where(is.numeric), round, 1)) %>% 
  as.data.frame()

# tabla
library(kableExtra)
impact_change %>% 
  filter(woman_age %in% seq(15,50,5))%>%
  rename(`Age mother`=1,`N Childs`=2, `Prop. Age`=3, `Mean Age`=4, `-.0001`=5, `-.0002`=6, `-.003`=7,
                                   `Prop. Age `=8, `Mean Age `=9, `-.0001 `=10, `-.0002 `=11, `-.003 `=12) %>% 
  replace(is.na(.), ".") %>% 
  kable() %>% 
  kable_classic() %>% 
  kable_paper(bootstrap_options = "striped", full_width = F) %>% 
  add_header_above(c(" " = 2, "Age 0" = 5, "Ages 15-24" = 5)) %>% 
  save_kable("R/table_impact.png")
  