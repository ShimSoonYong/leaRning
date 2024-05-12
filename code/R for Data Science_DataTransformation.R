library(nycflights13)
library(tidyverse)

flights


# filter
flights %>%
  filter(month==1,day==1)
jan1<-filter(flights,month==1,day==1)
(dec25<-filter(flights,month==12,day==25))

sqrt(2)^2==2
1/49*49==1
near(sqrt(2)^2,2)
near(1/49*49,1)

df<-tibble(x=c(1,NA,3))
filter(df,x>1)
filter(df,is.na(x)|x>1)

# arrange
flights %>%
  arrange(year,month,day)

flights%>%
  arrange(desc(arr_delay))

df<-tibble(x=c(5,2,NA))
df%>%arrange(x)
df%>%arrange(desc(x))

# select
flights %>%
  select(year,month,day)
flights%>%
  select(year:day)
flights%>%select(-(year:day))

# rename
flights%>%rename(tail_num=tailnum)
flights%>%select(time_hour, air_time, everything())


### mutate ###
flights_sml<-flights%>%
  select(year:day, ends_with("delay"),
         distance,
         air_time)
flights_sml%>%
  mutate(gain=arr_delay-dep_delay,
         speed=distance/air_time*60)
flights_sml%>%
  mutate(gain=arr_delay-dep_delay,
         hours=air_time/60,
         gain_per_hour=gain/hours)
flights%>%
  transmute(gain=arr_delay-dep_delay,
            hours=air_time/60,
            gain_per_hour=gain/hours)


### summarise ###
flights%>%
  summarise(delay=mean(dep_delay,na.rm = T))
flights%>%
  group_by(year,month,day)%>%
  summarise(delay=mean(dep_delay,na.rm = T))


flights%>%
  group_by(dest)%>%
  summarise(
    count=n(),
    dist=mean(distance,na.rm = T),
    delay=mean(arr_delay,na.rm = T)
  )%>%
  filter(count>20,dest!="HNL")

ggplot(flights%>%
         filter(!is.na(dep_delay),!is.na(arr_delay))%>%
         group_by(tailnum)%>%
         summarise(delay=mean(arr_delay))
         , aes(delay))+
  geom_freqpoly(binwidth=10)

flights%>%
  filter(!is.na(dep_delay),!is.na(arr_delay))%>%
  group_by(tailnum)%>%
  summarise(delay=mean(arr_delay),
            n=n())%>%
  ggplot(aes(x=n,y=delay))+
  geom_point(alpha=1/10)

batting<-as_tibble(Lahman::Batting)

batting%>%
  group_by(playerID)%>%
  summarise(
    ba=sum(H,na.rm = T)/sum(AB,na.rm = T),
    ab=sum(AB,na.rm=T)
  )%>%
  filter(ab>100)%>%
  ggplot(
    aes(
      x=ab,
      y=ba
    )
  )+
  geom_point()+
  geom_smooth(se=F)
  
# ungroup
flights %>%
  group_by(day)%>%
  ungroup()%>%
  summarise(flights=n())



#### grouping mutate and filter ####
flights_sml%>%
  group_by(year,month,day)%>%
  filter(rank(desc(arr_delay))<10)

(popular_dests<-flights%>%
    group_by(dest)%>%
    filter(n()>365))

popular_dests%>%
  filter(arr_delay>0)%>%
  mutate(prop_delay=arr_delay/ sum(arr_delay))%>%
  select(year:day, dest, arr_delay, prop_delay)


