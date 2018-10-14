pa1min_: Minutes Of Total Physical Activity Per Week
bpmeds: Currently Taking Blood Pressure Medication

summary(brfss2013$pa1min_)
hist(brfss2013$pa1min_[which(brfss2013$pa1min_ < 6720)], breaks = 100)

sum(brfss2013$pa1min_ > 6720, na.rm = T)

q2 <- brfss2013 %>% 
  filter(pa1min_ < 1000)

boxplot(q2$pa1min_)
boxplot(q2$pa1min_[which(q2$bpmeds == 'Yes')])
boxplot(q2$pa1min_[which(q2$bpmeds == 'No')])

ggplot(q2, aes(x = bpmeds, y = pa1min_)) +
  geom_boxplot()

brfss2013$X_minac11[which(brfss2013$pa1min_ > (60*24*7))]
plot(density(brfss2013$X_minac11[which(brfss2013$pa1min_ > 6000)], na.rm = T))


8*60*5

5000/60
6000
