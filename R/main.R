library(palmerpenguins)
library(tidyverse)

glimpse(penguins)

long <- penguins |> 
  select(species,
         sex,
         bill_length_mm,
         bill_depth_mm) |> 
  pivot_longer(cols = c("bill_length_mm", "bill_depth_mm"),
               names_to = "measurement", values_to = "length") |> 
  group_by(species,
           sex,
           measurement) |> 
  summarise(avg_length = mean(length, na.rm = TRUE)) |> 
  filter(!is.na(sex))

long |> 
  mutate(measurement = ifelse(measurement == "bill_depth_mm",
                              "Avg. Bill\nDepth",
                              "Avg. Bill\nLength"),
         sex = ifelse(sex == "female", "Female", "Male")) |> 
  ggplot(aes(measurement, avg_length, fill = sex)) +
  geom_col(position = "dodge",
           width = .75) +
  scale_x_discrete(expand = c(.5, .5)) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  facet_wrap(~ species, strip.position = "bottom") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "cm"),
        legend.position = "bottom",
        strip.text = element_text(size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.title = element_text(size = 24, face = "bold",
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(size = 14, 
                                     color = "grey30",
                                     margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0,
                                    color = "grey50",
                                    size = 12)) +
  labs(fill = "", x = "", y = "Size (mm)",
       caption = "Data from {palmerpenquins} R package.",
       title = "Comparative Bill Sizes",
       subtitle = "Dimensions for male and female Adelie, Chinstrap, and Gentoo penguins at Palmer Station LTER.")

ggsave("plots/final_plot.png", bg = "white")
