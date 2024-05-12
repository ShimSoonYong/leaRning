library(tidyverse)
ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy))
head(mpg)

ggplot(data = mpg) + 
  geom_point(aes(class, drv))


ggplot(data = mpg)+
  geom_point(aes(x=displ, hwy, color=class))

ggplot(data = mpg)+
  geom_point(aes(displ, hwy, size=class))
ggplot(data = mpg)+
  geom_point(aes(displ, hwy, alpha=class))
ggplot(data = mpg)+
  geom_point(aes(displ, hwy, shape=class))

ggplot(data=mpg)+geom_point(aes(displ,hwy),color="blue")


### facet! ###
ggplot(data=mpg)+
  geom_point(aes(displ,hwy))+
  facet_wrap(~class, nrow=2)
ggplot(data=mpg)+
  geom_point(aes(displ,hwy))+
  facet_grid(drv~cyl)
ggplot(data = mpg) +  
  geom_point(mapping = aes(x = displ, y = hwy)) +  
  facet_grid(drv ~ .)
ggplot(data = mpg) +  
  geom_point(mapping = aes(x = displ, y = hwy)) +  
  facet_wrap(~ class, nrow = 2)


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +  
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +  
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +  
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +  
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),
              show.legend = FALSE)

ggplot(data = mpg) +  
  geom_point(mapping = aes(x = displ, y = hwy)) +  
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +  
  geom_point() + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) +  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(    
    data = filter(mpg, class == "subcompact"),    
    se = FALSE  )

ggplot(mpg, aes(displ, hwy))+
  geom_point()+geom_smooth()
ggplot()+
  geom_point(data=mpg, aes(displ,hwy))+
  geom_smooth(data=mpg, aes(displ,hwy))


### stat! ###
ggplot(diamonds)+
  geom_bar(aes(cut))

ggplot(diamonds)+
  stat_count(aes(cut))

demo<-tribble(~cut, ~freq,
              "Fair", 1610,
              "Good", 4906,
              "Very Good", 12082,
              "Premium", 13791,
              "Ideal", 21551)
ggplot(demo)+
  geom_bar(aes(cut,freq), stat = "identity")

ggplot(diamonds)+
  geom_bar(aes(cut,stat(prop),group=1))

ggplot(diamonds)+
  stat_summary(aes(cut,depth),
               fun.ymin = min,
               fun.ymax = max,
               fun.y = median)


### Handling Position! ###
ggplot(diamonds)+
  geom_bar(aes(cut,color=cut))
ggplot(diamonds)+
  geom_bar(aes(cut,fill=cut))
ggplot(diamonds)+
  geom_bar(aes(cut,fill=clarity))

ggplot(diamonds,
       aes(cut,fill=clarity))+
  geom_bar(alpha=1/5, position = "identity")
ggplot(diamonds,
       aes(cut,color=clarity))+
  geom_bar(fill=NA,position = "identity")

ggplot(diamonds)+
  geom_bar(aes(cut,fill=clarity),
           position="fill")
ggplot(diamonds)+
  geom_bar(aes(cut,fill=clarity),
           position="dodge")

ggplot(mpg)+
  geom_point(aes(displ,hwy),
             position="jitter")


### Coordinate system! ###
ggplot(mpg, aes(class, hwy))+
  geom_boxplot()
ggplot(mpg, aes(class, hwy))+
  geom_boxplot() + coord_flip()

nz<-map_data("nz")
ggplot(nz, aes(long, lat, group=group))+
  geom_polygon(fill="green", color="black")
ggplot(nz,aes(long,lat, group=group))+
  geom_polygon(fill="green",color="red")+
  coord_quickmap()

bar<-ggplot(diamonds)+
  geom_bar(aes(cut,fill=cut),
           show.legend = F,
           width=1)+
  theme(aspect.ratio = 1)+
  labs(NULL,NULL)
bar+coord_flip()
bar+coord_polar()
