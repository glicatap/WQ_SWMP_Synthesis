#' making up numbers to illustrate an idea  
#' 

library(tidyverse)

seas_df <- data.frame(
  domain = c("wq", "wq", "wq", "nut", "nut", "nut"),
  param = c("do-pct", "do-mgl", "temp", "chla", "no23", "po4"),
  sea_yes = c(100, 100, 118, 103, 40, 60)
)
seas_df <- seas_df |> 
  mutate(sea_no = 120 - sea_yes,
         sea_no = -1*sea_no) |> 
  arrange(desc(domain),
          desc(sea_yes)) |> 
  mutate(param = factor(param, param))

ggplot(seas_df) +
  geom_segment( aes(x=param, xend=param, y=sea_no, yend=sea_yes,
                    color = domain)) +
  geom_point( aes(x=param, y=sea_no, color="seasonal"), size=3 ) +
  geom_point( aes(x=param, y=sea_yes, color="not seasonal"), size=3 ) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette = "Set1") +
  coord_flip()+
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("# of stations displaying seasonality")
