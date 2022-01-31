source("../CODE/LICOR_clean_fxns.R")
require(tidyverse)

d <- MegaFrame()
d <- LC(d)
d2 <- d %>% filter(H2OR < 15) %>%
  filter(Ci >0 & Ci < 700) %>% 
  


ggplot(d , aes(x = Ci, y = Photo))+
  geom_line(aes(color = comment))
