##################
### libraries: ###
##################

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggtext)
library(scales)

#####################
### data loading: ###
#####################

tuesdata <- tidytuesdayR::tt_load('2023-02-21')
bob_ross <- tuesdata$bob_ross
rm(tuesdata)

#########################################################
### select variables for the correspondence analysis: ###
#########################################################

selection <- bob_ross[, sapply(bob_ross, is.logical)] %>% 
  lapply(as.numeric) %>% 
  data.frame() %>% 
  bind_cols(bob_ross %>% 
              select(painting_title))

########################################
### conduct correspondence analysis: ###
########################################

res.ca <- CA(selection[,1:18],
             graph = FALSE)

### getting to know the % of variance explained by the top 2 dimensions:
#res.ca$eig %>% 
#  head()

################
### drawing: ###
################

plot <- res.ca$col$coord %>% 
  as.data.frame() %>% 
  select(`Dim 1`,
         `Dim 2`) %>% 
  
  mutate(color_name = row.names(.)) %>% 
  `rownames<-`(c(1:18)) %>% 
  
  left_join(c('Alizarin Crimson' = '#4E1500',
              'Bright Red' = '#DB0000',
              'Cadmium Yellow' = '#FFEC00',
              'Phthalo Green' = '#102E3C',
              'Prussian Blue' =  '#021E44',
              'Sap Green' =  '#0A3410',
              'Titanium White' = 'black',#'#FFFFFF',
              'Van Dyke Brown' = '#221B15',
              'Black Gesso' = '#000000',
              'Burnt Umber' = '#8A3324',
              'Indian Yellow' = '#FFB800',
              'Phthalo_Blue' = '#0C0040',
              'Yellow Ochre' = '#C79B00',
              'Dark Sienna' = '#5F2E1F',
              'Midnight Black' = '#000000',
              'Liquid_Clear' = 'black', # it is '#FFFFFF' originally, so it would not be depicted
              'Liquid_Black' = '#000000',
              'Indian_Red' = '#CD5C5C') %>%
              as.data.frame() %>%
              `colnames<-`("hex") %>%
              mutate(type = "col",
                     color_name = rownames(.)) %>%
              mutate(color_name = str_replace_all(color_name,
                                                  " ",
                                                  "_")) %>% 
              `rownames<-`(c(1:18)),
            by = "color_name") %>% 
  
  bind_rows(res.ca$row$coord %>% 
              as.data.frame() %>% 
              select(`Dim 1`,
                     `Dim 2`) %>% 
              mutate(type = "row",
                     color_name = NA,
                     hex = "#000000")) %>% 
  mutate(dim1_scaled = rescale(`Dim 1`),
         dim2_scaled = rescale(`Dim 2`)) %>% 
  
  filter(dim2_scaled < 0.176 & ## 1 color + 17 obs.
           dim2_scaled > 0.11 & ## 3 colors
           dim1_scaled > 0.15 & ## 1 color + 4 obs.
           dim1_scaled < 0.45) %>% ## 1 color & 55 obs  
  ggplot(aes(dim1_scaled,
             dim2_scaled,
             shape = type,
             size = type,
             color = hex)) +
  geom_point() +
  
  scale_colour_identity() + ## make R identify the color codes in the data
  scale_size_manual(values = c(6, 0.5)) +
  scale_shape_manual(values = c(20, 4)) +
  scale_x_continuous(breaks = c(0.2, 0.3, 0.4),
                     limits = c(0.15, 0.45)) +
  scale_y_continuous(limits = c(0.115, 0.175)) +
  
  labs(title = "<span style='font-size:12pt'>**Correspondence analysis of colors used in Bob Ross paintings**</span><span style='font-size:10pt'><br>86 pictures and 6 colors omitted for clarity<br>omitted colors depicted with arrows (length does not represent remoteness)</span>",
       x = "dim-1 (*18.7% of variance*)",
       y = "dim-2 (*14.7% of variance*)",
       caption = "plotted by: **Arthur Pecherskikh** | github: @artpech23<br>Jared Wilber's data on **Bob Ross Paintings**<br>**#TidyTuesday**") +
  #geom_text(aes(label = color_name)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        axis.title.y = element_markdown(size = 10,
                                        hjust = 0),
        axis.title.x = element_markdown(size = 10,
                                        hjust = 0),
        plot.caption = element_markdown(size = 6),
        axis.text = element_text(size = 6)) +
  annotate("segment", 
           x = 0.4, 
           xend = 0.45, 
           y = 0.12, 
           yend = 0.115, 
           colour = "#8A3324", 
           size = 0.7, 
           arrow=arrow()) +
  
  annotate("segment", 
           x = 0.22, 
           xend = 0.22, 
           y = 0.125, 
           yend = 0.115, 
           colour = "#000000", 
           size = 0.7, 
           arrow=arrow()) +
  
  annotate("segment", 
           x = 0.37, 
           xend = 0.38, 
           y = 0.16, 
           yend = 0.174, 
           colour = "#000000", 
           size = 0.7, 
           arrow=arrow()) +
  annotate("segment", 
           x = 0.4, 
           xend = 0.45, 
           y = 0.152, 
           yend = 0.16, 
           colour = "#102E3C", 
           size = 0.7, 
           arrow=arrow()) +
  annotate("segment", 
           x = 0.2, 
           xend = 0.176, 
           y = 0.13, 
           yend = 0.115, 
           colour = "#CD5C5C", 
           size = 0.7, 
           arrow=arrow()) +
  annotate("segment", 
           x = 0.2, 
           xend = 0.17, 
           y = 0.16, 
           yend = 0.168, 
           colour = "black", 
           size = 0.7, 
           arrow=arrow()) +
  annotate("text",
           x = 0.18,
           y = 0.16,
           label = "liquid clear",
           fontface = "italic",
           size = 3)

###############
### saving: ###
###############

png("bob_ross_CA.png",
    width     = 8,
    height    = 4,
    units     = "in",
    res       = 800,
    pointsize = 4
)
print(plot)
dev.off()


