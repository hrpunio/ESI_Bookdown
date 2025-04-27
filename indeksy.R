library("dplyr")
library("tidyverse")
dane0 <- read.csv(file= "MZM.csv", header=T, sep=";") %>%
  select (data, zwiedzający=razem);

dane <- dane0 %>%
  mutate (year = substr(data, 1, 4)) %>%
  filter (as.numeric(year) < 2017) %>%
  mutate (przyrost =  zwiedzający - lag(zwiedzający, 1),
          przyrostw = ( zwiedzający - lag( zwiedzający, 1))/lag( zwiedzający, 1) * 100,
          indeks =  zwiedzający /lag( zwiedzający, 1) * 100,
          ###
          przyrostst =  zwiedzający - first(zwiedzający),
          przyrostwst = ( zwiedzający - first( zwiedzający))/first(zwiedzający) * 100,
          indeksst =  zwiedzający /first(zwiedzający) * 100
        )


library  ("eurostat")
library ("DescTools")
#install.packages("readr")
##
selected <- c('PL', 'DE')
excluded <- c('EU27_2020', 'IS', 'MT', 'CY', 'LU')

##https://ec.europa.eu/eurostat/databrowser/view/for_remov/default/table?lang=en
f0 <- get_eurostat("for_remov",  stringsAsFactors = FALSE) %>%
  filter (! geo %in% excluded ) %>%
  filter (treespec == 'TOTAL' & bark == 'UNBK' & prod_wd == 'RW' & unit == 'THS_M3')  %>%
  mutate (year = as.numeric(substr(TIME_PERIOD, 1,4)) ) %>%
  select (geo, year, rw=values) %>%
  mutate (geo=as.factor(geo))

f0s <- f0 %>%  filter (geo %in% selected )

##g0 <- f0 %>% select (year, rw) %>%
##  group_by(year) %>% summarise (g = Gini(rw, unbiased=F))

p1 <- f0s %>% ggplot(aes(x=year, y=rw, color=geo)) +
  geom_line(linewidth=1) +
  geom_vline(xintercept = 2008)
p1

years <- c(2000, 2010, 2020)

p1a <- f0 %>%
  filter (year %in% years) %>%
  ggplot(aes(x=reorder(geo, rw), y=rw)) +
  facet_wrap(~year) +
  geom_bar(stat="identity", fill='forestgreen') +
  ylab ("Ths cubic meters") +
  xlab ("Country") +
  ggtitle("Roundwood production (ths cubic meters)") +
  coord_flip()

p1a


i0 <- f0s %>%
  filter (as.numeric(year) > 1999) %>%
  group_by(geo) %>%
  summarise (
          year = year,
          i =  rw /lag( rw, 1) * 100,
          ic =  rw /first(rw) * 100
  )


##Forest
## Thousand cubic metres = THS_M3
f0s <- get_eurostat("for_vol_efa",  stringsAsFactors = FALSE) %>%
  filter (indic_fo == 'FOR' & stk_flow == 'STK_CL' & unit == 'THS_M3') %>%
  mutate (year = as.numeric(substr(TIME_PERIOD, 1,4)) ) %>%
  select (geo, year, available=values) %>%
  mutate (geo=as.factor(geo))

f1 <- f0 %>%  left_join(f0s, by=c('geo', 'year')) %>%
  mutate (rw = rw/available * 100)

## DE/PL
f1s.x <- f1 %>% filter (geo %in% selected )

p2 <- f1s.x %>% ggplot(aes(x=year, y=rw, color=geo)) +
  geom_smooth(linewidth=1, se = F) +
  geom_point(alpha=.4) +
  ylab ("%") +
  ggtitle("Production as % of available woodstock (PL vs DE)") +
  geom_vline(xintercept = 2008)
p2

##years <- c(2000, 2010, 2020)

p3 <- f1 %>% filter (year == 2000 | year == 2010 | year == 2020 ) %>%
  ggplot(aes(x=reorder(geo, rw), y=rw)) +
  facet_wrap(~year) +
  geom_bar(stat="identity", fill='forestgreen') +
  ylab ("%") +
  ggtitle("Production as % of available woodstock") +
  coord_flip()
p3
