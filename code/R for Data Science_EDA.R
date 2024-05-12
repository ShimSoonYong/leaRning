# 1. Make a question about data.
# 2. Answer the question by visualization, transformation, and modeling.
# 3. Improve a question or make a new question.
library(tidyverse)

ggplot(diamonds,
       aes(price))+
  geom_freqpoly(
    aes(color=cut),
    binwidth=500
  )
ggplot(diamonds,
       aes(price,..density..))+
  geom_freqpoly(
    aes(color=cut),
    binwidth=500
  )
ggplot(diamonds,
       aes(cut,price))+geom_boxplot()

ggplot(mpg)+
  geom_boxplot(
    aes(
      reorder(class, hwy, FUN=median),
      hwy
    )
  )+ coord_flip()


# For categorical variable
ggplot(diamonds)+
  geom_count(
    aes(cut,color)
  )
diamonds%>%
  count(color,cut)%>%
  ggplot(aes(color,cut))+
  geom_tile(aes(fill=n))+
  scale_fill_gradient(low="blue", high="red")


ggplot(faithful)+
  geom_point(aes(eruptions, waiting))

library(modelr)
mod<-lm(log(price)~log(carat),data=diamonds)

diamonds2<-diamonds%>%
  add_residuals(mod)%>%
  mutate(resid=exp(resid))
ggplot(data = diamonds2)+
  geom_point(aes(carat,resid))
ggplot(diamonds2)+
  geom_boxplot(aes(cut,resid))


ggplot(diamonds, aes(carat,price))+
  geom_hex()
ggsave("diamonds.pdf")
write_csv(diamonds, "diamonds.csv")
