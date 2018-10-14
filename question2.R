# ssbsugar: How Often Do You Drink Regular Soda Or Pop?
# ssbfrut2: How Often Did You Drink Sugar-Sweetened Drinks?
# sleptim1: How Much Time Do You Sleep
# On average, how many hours of sleep do you get in a 24-hour period?


# decide: Difficulty Concentrating Or Remembering


sugar <- brfss2013 %>% 
  select(ssbsugar,
         ssbfrut2,
         sleptim1,
         decide)

summary(sugar)
boxplot(sugar$sleptim1)

sugar$sleptim1[which(sugar$sleptim1 > 24)]

sugar <- sugar[-which(sugar$sleptim1 > 24),]

hist(sugar$sleptim1,
     col = 'peachpuff',
     border = 'black', 
     prob = TRUE,
     xlab = 'Body Mass Index with 2 implied decimal places',
     main = 'Histogram of Body Mass Index')
abline(v = mean(sugar$sleptim1, na.rm = T), # mean
       col = 'royalblue',
       lwd = 2)
abline(v = median(sugar$sleptim1, na.rm = T), # median
       col = 'red',
       lwd = 2)
legend(x = 'topright', # legend
       c('Mean', 'Median'),
       col = c('royalblue', 'red'),
       lwd = c(2, 2))


sugar <- sugar %>% 
  filter(!is.na(sleptim1),
         !is.na(decide))

# Histogram Colored (blue and red)
hist(sugar$sleptim1[sugar$decide == 'No'],
     col = rgb(1,0,0,0.5),
     #xlim = c(0,10), 
     #ylim = c(0,200), 
     main = 'title', 
     xlab = 'variable')
hist(sugar$sleptim1[sugar$decide == 'Yes'], 
     col = rgb(0,0,1,0.5), 
     add = T)
box()

plot(sugar$ssbsugar, sugar$sleptim1)
