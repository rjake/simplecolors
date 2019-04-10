setwd("J:/COLORS")

library(grDevices)
library(tidyverse)
library(colorspace)

# how many gradients?
nGradients <- 11

color_values <-
   #all colors
    expand.grid(H330 = seq(0, 330, round(330/nGradients, 0)),
                S1 = c(.25, .5, .75, 1),
                L1 = c(.2, .3, .45, .6, .75, .9),
                KEEP.OUT.ATTRS = F, stringsAsFactors = F)%>%
   #add grey scale
    rbind(expand.grid(H330 = 0,
                      S1 = 0,
                      L1 = c(.2, .3, .45, .6, .75, .9),
                      KEEP.OUT.ATTRS = F, stringsAsFactors = F)) %>%
   #add white & black
   # rbind(c(0, 0, 0)) %>% 
   # rbind(c(360, 0, 1)) %>%
    #get HEX & RGB codes
    rowwise() %>%
    mutate(H1 = round(H330/360, 2),
           HEX =  hex(HLS(H330, L1, S1)),
           R = col2rgb(HEX)[1],
           G = col2rgb(HEX)[2],
           B = col2rgb(HEX)[3],
           H255 = round(H1*255, -1),
           S255 = round(S1*255, -1),
           L255 = round(L1*255, -1)) %>%
    ungroup()%>%
    arrange(H255, S255, L255)

write.csv(color_values, "colors.csv", row.names = F)


ggplot(color_values) +
  facet_grid(~H1, scales = "free_x") +
  geom_tile(aes(x = factor(S1*100), y = factor(L1*100), fill = HEX)) +
  scale_fill_identity() +
  #coord_fixed(ratio = 1) +
  labs(x = "Saturation",
       y = 'Light',
       title = "Hue as decimals")


ggplot(color_values) +
  facet_grid(~H255, scales = "free_x") +
  geom_tile(aes(x = factor(S255), y = factor(L255), fill = HEX)) +
  scale_fill_identity() +
  labs(x = "Saturation",
       y = 'Light',
       title = "Hue on 255 scale")
