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

