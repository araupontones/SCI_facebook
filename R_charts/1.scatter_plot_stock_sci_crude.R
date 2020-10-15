source("R/set_up.R")

#'read data

data_un_sci = import(file.path(dir_clean, "joint_un_sci_clean.rds"))

names(data_un_sci)
ggplot(data = data_un_sci,
       aes(x = log(Stock),
           y = log(scaled_sci))
)+
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stock (log)",
       y = "Facebook SCI (log)")

