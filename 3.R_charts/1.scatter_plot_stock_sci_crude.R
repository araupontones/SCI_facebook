source("set_up.R")

#'read data

#data_un_sci = import(file.path(dir_clean, "joint_un_sci_clean.rds"))

data_un_sci = import(file.path(dir_clean, "joint_un_sci_pop_codelist.rds")) %>%
  filter(stocks_bilateral >100,
         seq == 1) %>%
  mutate(flow_income = str_replace(flow_income, "income", ""))
 


ggplot(data = data_un_sci,
       aes(x = log(stocks_bilateral),
           y = log(scaled_sci))
)+
  geom_point(aes(fill = flow_income),
             show.legend = F,
             size = 1,
             color = "white",
             shape = 21) +
  geom_smooth(method = "lm",
              color = "#0033A1") +
  facet_wrap(
    ~flow_income, scales = c("free_y")
  ) +
  labs(x = "UN's Stock of Migrants (log)",
       y = "Facebook SCI (log)",
       title= "Correlation between Facebook SCI and UN bilateral stock") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 12, family = "Open Sans"),
    axis.title = element_markdown(family="Open Sans Light", size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#E3E3E3"),
    plot.title = element_markdown(family = "Roboto", hjust = .5, size = 20)
  )

