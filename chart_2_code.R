library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(ggtext)

spreadsheet_details <- gs4_get("1wszXLd_WTDRzjNdOAbsCjPJDCmrfVgol-Vb7QaU2D9U")

# Chart 2: Amazon's dominance in the shipping market ----------------------

chart_2_data <- read_sheet(
  ss = spreadsheet_details,
  sheet = "Amazon shipping",
  range = "A6:C10"
) %>% 
  rename(company = `...1`, `2019` = `2019 deliveries`, `2020` = `2020 deliveries`)

chart_2_data_longer <- 
  chart_2_data %>% 
  pivot_longer(-company, values_to = "deliveries", names_to = "year") %>% 
  mutate(
    color = if_else(company == "Amazon Logistics", "#e63946ff", "#a8dadcff")
  )

ggplot(chart_2_data_longer, aes(year, deliveries, color = color)) +
  geom_line(aes(group = company), size = 1.05) +
  geom_line(
    data = filter(chart_2_data_longer, company == "Amazon Logistics"),
    aes(group = company), size = 1.05
  ) +
  geom_point(size = 4) +
  geom_label(aes(x = "2019", y = 8, label = "8 billion"), hjust = 0.3,
             vjust = -0.1, family = "Work Sans", size = 4.5, 
            fontface = "bold", color = "#D2D2D2", fill = "#F0F0F0", label.size = 0) +
  geom_text(
    data = filter(chart_2_data_longer, year == 2020),
    aes(label = company), hjust = 0, nudge_x = 0.05,
    family = "Work Sans", fontface = "bold", size = 4.5
  ) +
  scale_x_discrete(expand = expansion(add = 0.06)) +
  scale_y_continuous(limit = c(NA, 8), labels = c(2, 4, 6, ""), 
                     expand = expansion(mult = 0.05)) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  labs(
    title = "<span style='color: #e63946ff'>Amazon's</span> dominance over the U.S.<br>shipping market is growing",
    subtitle = "Number of packages delivered"
  ) +
  theme_fivethirtyeight(base_family = "Work Sans", base_size = 14) +
  theme(
    plot.margin = margin(20, 120, 20, 20),
    plot.title.position = "plot",
    plot.title = element_markdown(size = rel(1.5), lineheight = 1.1),
    plot.subtitle = element_text(size = rel(0.9), color = "#D2D2D2", 
                                 margin = margin(t=  10, b = 5), face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(size = rel(1.4), face = "bold"),
    axis.text.y = element_text(vjust = -0.5, margin = margin(r = -10),
                               color = "#D2D2D2", face = "bold", size = rel(1.1))
  ) 
ggsave("plots/chart_2_amazon_shipping.png", width = 6, height = 8, dpi = 300)
