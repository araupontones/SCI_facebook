source("set_up.R")
source("3.R_charts/style.R")
#read data
joint_data = import(file.path(dir_clean, "joint_un_sci_pop_codelist.rds"))


data_un_sci = joint_data %>%
  filter(stocks_bilateral >0)

export(data_un_sci %>%
         mutate(stocks_bilateral = log(stocks_bilateral),
                scaled_sci = log(scaled_sci)
                ), file.path(dir_plots, "data.csv"))


data_points= subset(data_un_sci, ID_direction%in% c("MXUS", "LRSL", "NGNO", "MAES", "INAR",
                                                    "PGWS")) %>%
  mutate(Destination = str_replace_all(Destination,"United States of America", "U.S"),
         label = paste0(Origin, "-",Destination))

data_points_above= subset(data_un_sci, ID_direction%in% c("MXUS", "LRSL", "PGWS")) %>%
  mutate(Destination = str_replace_all(Destination,"United States of America", "U.S"),
         Origin = str_replace_all(Origin,"United Republic of Tanzania", "Tanzania"),
         
         label = paste0(Origin, "-",Destination))


unique(data_points_above$Destination)

data_points_white = subset(data_un_sci, ID_direction%in% c("NGNO")) %>%
  mutate(Destination = str_replace_all(Destination,"United States of America", "U.S"),
         label = paste0(Origin, "-",Destination))

data_points_below = subset(data_un_sci, ID_direction%in% c("MAES", "INAR")) %>%
  mutate(Destination = str_replace_all(Destination,"United States of America", "U.S"),
         label = paste0(Origin, "-",Destination))



plot = ggplot(data = data_un_sci,
       aes(x = log(stocks_bilateral, base = 10),
           y = log(scaled_sci))
)+
  #define hex -------------------------------------------------------------------
  geom_hex(bins = 42,
           #color = "white",
           size =.5
           )+
  #define linear model line ----------------------------------------------------
  geom_smooth(
              color = "black",
              size = 2
              ) +
  #Points ------------------------------------------------------------------------
  geom_point(data= data_points,
             size = 3,
             color = "white",
             fill = "black",
             shape = 21
             
             ) +
  #text above points ------------------------------------------------------------
  geom_text(data= data_points_above,
             aes(label = label),
            hjust = .5,
            nudge_y = .5,
            family="Open Sans Light",
            fontface = 'bold'
             
  ) +
  #text in gray points ------------------------------------------------------------
  geom_text(data= data_points_white,
            aes(label = label),
            hjust = .5,
            nudge_y = .5,
            family="Open Sans Light",
            fontface = 'bold',
            color = "#E3E3E3"
            
  ) +
  #text below points ------------------------------------------------------------
  geom_text(data= data_points_below,
            aes(label = label),
            hjust = .8,
            nudge_y = -.5,
            family="Open Sans Light",
            fontface = 'bold'
            
  ) +
  #Y Axis ----------------------------------------------------------------------
  scale_y_continuous(limits = c(2,18)) +
  # X axis ----------------------------------------------------------------------
  scale_x_continuous(
    labels = function(x){prettyNum(10^x, big.mark = ",")}
    ) +
  #legend -----------------------------------------------------------------------
  scale_fill_gradient(
    low="#FDE2E4",high="#A6112C",trans="log10"
    ) +
  guides(fill = guide_colourbar(barwidth = 10, barheight = .7, ticks.linewidth = 1)
         ) +
 
  #legend text -----------------------------------------------------------------
  annotate('text',
           y = 18,
           x = 6,
           #hjust = .5,
           label = "Number of observations",
           family="Open Sans Light") +

  annotate('text',
           y = 17.2,
           x = 4.93,
           hjust = 1,
           label = "0",
           family="Open Sans Light"

           ) +

  annotate('text',
           y = 17.2,
           x = 7.4,
           hjust = 0,
           label = "30",
           family="Open Sans Light"
           ) +
  #labels ======================================================================
  labs(y = "Facebook Social Connectedness Index* ( _log scale_)",
       x = "Bilateral Stock of Migrants* ( _log scale_)",
       caption = "**Data**: United Nations Population Division, 2017 | Facebook SCI, November 2020", 
       title = "Facebook SCI and UN's Bilateral Stock of Migrants") +
  
  #theme ------------------------------------------------------------------------
  theme(
   
    #Panel
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#E3E3E3"),
    
     #title
    legend.title = element_blank(),
    
    #legend
    legend.text = element_blank(),
    legend.position = c(.8,.9),
    legend.direction = 'horizontal',
    
    #axis
    axis.title = element_markdown(family="Open Sans Light", size = 16),
    axis.text = element_markdown(size =12, family="Open Sans Light"),
    axis.title.x = element_markdown(margin = margin(t =5)),
    axis.title.y = element_markdown(margin = margin(r =5)),
    axis.ticks = element_blank(),
    
    #caption 
    plot.caption = element_markdown(family="Open Sans Light", size = 12, margin = margin(t = 10)),
    
    #title 
    plot.title = element_markdown(family="Open Sans Light", size = 24, hjust = .1)
    
   
    
  ) 


plot

filename = file.path(dir_plots, "SCI_UN.png")

ggsave(filename = filename,
       plot = plot,
       dpi = 400) 
