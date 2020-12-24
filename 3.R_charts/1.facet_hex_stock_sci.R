source("set_up.R")
source("3.R_charts/style.R")


#'read data

#data_un_sci = import(file.path(dir_clean, "joint_un_sci_clean.rds"))

regions= c("Americas to Americas", 
           "Africa to Europe",
           "Asia to Americas")

incomes = c("High income to High income", 
            "Low income to High income", 
            "High income to Low income" )



data_un_sci = import(file.path(dir_clean, "joint_un_sci_pop_codelist.rds")) %>%
  filter(seq == 1) %>%
  #mutate(flow_income = str_replace(flow_income, "income", "")) %>%
  filter(flow_income %in% c("High income to High income", "Low income to High income", "High income to Low income")) %>%
  mutate(source = "income") %>%
  select(stocks_bilateral,scaled_sci, source, flow_income) %>%
  rename(category = flow_income)

data_un_sci_regions = import(file.path(dir_clean, "joint_un_sci_pop_codelist.rds")) %>%
  filter(seq == 1) %>%
  #mutate(flow_income = str_replace(flow_income, "income", "")) %>%
  filter(flow_region %in% regions) %>%
  mutate(source = "region")  %>%
  select(stocks_bilateral,scaled_sci, source, flow_region)%>%
  rename(category = flow_region)




data = rbind(data_un_sci, data_un_sci_regions)





plot = ggplot(data = data,
       aes(x = log(stocks_bilateral, base = 10),
           y = log(scaled_sci))
)+
  geom_hex(bins = 15) +
  
  geom_smooth(
              color = "black",
              size = 1.5
              ) +
  facet_wrap(
    ~category #scales = c("free_y")
  ) +
  
  scale_fill_gradient(
    low="#FDE2E4",high="#A6112C",trans="log10", breaks= c(1,30)
  ) +
  scale_x_continuous(limits = c(1,6),
                      breaks = c(4,6),
                     labels = function(x){ prettyNum(10^x, big.mark = ",")})+
 
   guides(fill = guide_colourbar(barwidth = 6, barheight = .5, ticks.linewidth = 1,
                                title = "Observations", title.position = 'top',
                                title.hjust = .5, label.position = "bottom",
                                label.hjust = 0)
  ) +

  #scale_x_continuous()+
  labs(y = "Facebook Social Connectedness Index",
       x = "Bilateral Stock of Migrants",
       title= "Correlation Between Facebook's SCI and UN's Bilateral Stock",
       subtitle = "By Countries' Region and Income",
       caption = "**Data**: United Nations Population Division, 2017 | Facebook SCI, November 2020 | **Chart:** Andres Arau") +
  theme(
    
    #Panel
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#E3E3E3"),
    
    #legend
    legend.title = element_markdown(family = "Open Sans Light", size =10),
    legend.direction = 'horizontal',
    legend.position = c(.88,.9),
    legend.text = element_markdown(family = "Open Sans", size =8),
    legend.margin = margin(t=0, r = 0, b = 0, l = 0),
    
    #Strip
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12, family = "Open Sans Light"),
    
    #Axis
    axis.title = element_markdown(family="Open Sans Light", size = 16),
    axis.title.x = element_markdown(margin = margin(t =5)),
    axis.title.y = element_markdown(margin = margin(r =5)),
    axis.text.y = element_blank(),
    axis.text.x = element_markdown(size =10, family="Open Sans Light", hjust = 1),
    axis.ticks = element_blank(),
    
    
    #caption 
    plot.caption = element_markdown(family="Open Sans Light", size = 12, margin = margin(t = 10)),
    
    #title 
    plot.title = element_markdown(family="Open Sans Light", size = 24),
    plot.subtitle =  element_markdown(family="Open Sans Light", 
                                      size = 20, 
                                      margin = margin(b = 10),
                                      colour = "#55545C")
  )

plot

filename = file.path(dir_plots, "SCI_UN_facet.png")

ggsave(filename = filename,
       plot = plot,
       dpi = 400)
