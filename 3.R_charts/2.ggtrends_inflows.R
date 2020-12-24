source("set_up.R")
source("3.R_charts/style.R")


#'read and clean data ---------------------------------------------------------
target = import(file.path(dir_clean,"Target_list.RData"))

#append data 
syria = target$`Syria to Sweden` %>%
  mutate(combination = "Syria to Sweden")

iraq = target$`Iraq to Canada` %>%
  mutate(combination = "Iraq to Canada")


china = target$`China to Korea` %>%
  mutate(combination = "China to Korea")


data_gg = rbind(syria, iraq, china)



#' plot ggtrends vs inflows for the three country pairs------------------------

pairs = c("Iraq to Canada", "China to Korea")


#1 function to plot  
plot_pais =  function(p) {
  
  #print(p)
  
  #define axis parameters for each country pairs ----------------------------
  if(p!="China to Korea"){
    limits = c(0,100000)
    coeff = 1000
  } else {
    limits = c(0,200000)
    coeff = 2000
    
  }
  
  #subset data for each pair --------------------------------------------
  data = subset(data_gg, combination == p)
  
  #Create spylines so the line in the plot is smoother ----------------------
  spline_int <- as.data.frame(spline(data$Year, data$migration_flow_total))
  spline_int_hits <- as.data.frame(spline(data$Year, data$GT_hits))
  
  
  #Define the plot -----------------------------------------------------------
  plot = ggplot(
    data = subset(data_gg, combination == p),
    aes(x = as.numeric(Year),
        y = migration_flow_total,
        group = 1
    )
    
  ) +
    
    #spy line of inflows ----------------------------------------------------
  geom_line(data = spline_int,
            aes(
              x = x,
              y = y
              
            ),
            size =1.3,
            color = pink_un
  ) +
    #spy line of gg trends ---------------------------------------------------
  geom_line(data = spline_int_hits,
            aes(
              x = x,
              y = y *coeff
              
            ),
            size =1.1,
            color = blue_google,
            linetype = "dashed"
  ) +
    
    #title and caption --------------------------------------------------------
    
    labs(title = "Correlation Between Google Trends Index & OECD Migration Inflows",
         subtitle = p,
         caption = caption
         ) +
    
    #axis parameters (create right axis and define limits) ---------------------
    scale_y_continuous(
      
      name = "Migration Inflows",
      
      limits = limits,
      
      labels = function(x){prettyNum(x, big.mark = ",")},
      
      
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*1, 
                           name="Google Trends Index",
                           labels = function(x){x/coeff})
      
    ) +
    
    #Parameters of X axis -------------------------------------------------------
    scale_x_continuous(
      name = "Year",
      label = function(x){round(x,digits = 0)}
      ) +
    
    #theme ---------------------------------------------------------------------
    theme(
      
      
      #panel
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "#E3E3E3"),
      
      #title 
      plot.title = element_markdown(family="Open Sans Light", size = 20, hjust = .5),
      plot.subtitle = element_markdown(family="Open Sans Light", size = 16, hjust = .5,
                                       colour = "#313131", margin = margin(b = 10)),
      
      #caption 
      plot.caption = element_markdown(family="Open Sans Light", size = 12, margin = margin(t = 10)),
      
      
      #axis
      axis.ticks = element_blank(),
      axis.title = element_markdown(family="Open Sans Light", size = 16),
      axis.text = element_markdown(size =12),
      
      #style left axis
      axis.title.y.left =  element_markdown(color = pink_un,face = 'bold',
                                            margin = margin(r = 10)),
      axis.text.y.left = element_markdown(color = pink_un),
      
      #style right axis
      axis.title.y.right = element_markdown(color = blue_google,face = 'bold',
                                            margin = margin(l = 10)),
      axis.text.y.right = element_markdown(color = blue_google),
      
      #style x axis
      axis.title.x = element_markdown(margin = margin(t = 5),color = "#313131")
      
      
      
      
    )
  
  
  #save plot in local drive
  filename = file.path(dir_plots, paste0(p,".png"))
  print(filename)
  ggsave(filename, plot = plot, dpi = 400,
         width = 9,
         height = 4)
  #return the plot  
  return(plot)
  
  
}


#Create plots -----------------------------------------------------------------
map(pairs, plot_pais)
names(plots) = pairs


