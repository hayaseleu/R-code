data <- readRDS("small_earthquake.rds")
wether <- fread("weather.csv")
wether <- wether %>% rename_at(vars(starts_with("d")),.~ extract_numeric(.)) 
wether2 <- wether %>% gather(day,value,c(-id,-year,-month,-element), na.rm = T) 

wether3 <- as.data.table(wether2)
wether3[,day := as.numeric(day),]



wether3[1:30,year := 2012] 
wether2 <- wether3 %>% as.tbl() %>% arrange(year,month,day)
wether2 

ggplot(data = wether3) + 
  geom_point(mapping = aes(x = day , y = value , color = as.factor(year) ))


