acceptance_rate <- gss %>% 
  group_by(year, homosex) %>% 
  summarise(n = n()) %>% 
  group_by(year) %>% 
  mutate(rate = n / sum(n)) %>% 
  filter(homosex == 'Not Wrong At All')

ggplot(acceptance_rate, aes(x = year, y = rate)) +
  geom_line()

# Years not in the data
rep(1973:2012)[which(!rep(1973:2012) %in% acceptance_rate$year )]

gss %>% 
  filter(year %in% c(1972, 1975, 1978, 1983, 1986)) %>% 
  group_by(year, homosex) %>% 
  summarise(n = n())

# 1972 1975 1978  1983 1986 question was not in study
# 1979, 1981, or 1992: due to funding limitations, there were no GSSs in those years.
# 1995 1997 1999 2001 2003 2005 2007 2009 2011: Since 1994, the GSS has been conducted in even numbered years.

# INFERENCE
acceptance_rate_compare <- gss %>% 
  select(year, homosex) %>% 
  filter(year == 1973 |
           year == 2012)

acceptance_rate_compare$year <- factor(acceptance_rate_compare$year)

summary(acceptance_rate_compare)
acceptance_rate_compare %>% 
  group_by(year, homosex) %>% 
  summarise(n())

# check conditions for inference (MISSING)
inference(y = homosex, x = year, data = acceptance_rate_compare,
          type = 'ht',
          statistic = 'proportion',
          success = 'Not Wrong At All',
          method = 'theoretical',
          null = 0,
          alternative = 'twosided')
# do a confidence interval (MISSING)

# SECOND PART

summary(gss$class)


library(ggplot2)
library(ggmosaic)

education <- gss %>% 
  filter(year == 2012) %>% 
  select(homosex, class, degree) %>% 
  filter(!is.na(homosex),
         !is.na(degree))
  
class <- gss %>% 
  filter(year == 2012) %>% 
  select(homosex, class, degree) %>% 
  filter(!is.na(homosex),
         !is.na(class))

summary(education)

# Analysis plot
ggplot(data = education) +
  geom_mosaic(aes(x = product(homosex, degree), fill = homosex)) +
  labs(x = 'xxxx',
       y = 'yyyyy',
       title = 'tttt',
       fill = '') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Analysis plot
ggplot(data = class) +
  geom_mosaic(aes(x = product(homosex, class), fill = homosex)) +
  labs(x = 'xxxx',
       y = 'yyyyy',
       title = 'tttt',
       fill = '') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# facet_grid(~sex)