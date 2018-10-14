drnk3ge5:Binge Drinking
Considering all types of alcoholic beverages, how many times during the past 30 days did you have 5 or more drinks for men or 4 or more drinks for women on an occasion?
  
summary(brfss2013$drnk3ge5)
sum(brfss2013$drnk3ge5 > 30, na.rm = T)

drinks <- brfss2013 %>% 
  filter(drnk3ge5 <= 30)

summary(drinks$drnk3ge5)
hist(drinks$drnk3ge5)
boxplot(drinks$drnk3ge5)

ggplot(drinks, aes(x = sex, y = drnk3ge5)) +
  geom_boxplot() +
  coord_flip()

drinks_female <- drinks %>% 
  filter(sex == 'Female')

ggplot(drinks_female, aes(y = drnk3ge5)) +
  geom_boxplot() +
  coord_flip()

summary(drinks$drnk3ge5[which(drinks$sex == 'Female')])
