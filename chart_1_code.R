library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggthemes)
library(lubridate)
library(shadowtext)

spreadsheet_details <- gs4_get("1wszXLd_WTDRzjNdOAbsCjPJDCmrfVgol-Vb7QaU2D9U")

# Chart 1: Price increases for paper/cardboard ----------------------------

chart_1_data <- read_sheet(
  ss = spreadsheet_details,
  sheet = "Cardboard prices",
  range = "G9:I46"
) %>% 
  clean_names()

chart_1_data_longer <- 
  chart_1_data %>% 
  mutate(month = as.Date(month)) %>% 
  pivot_longer(cols = -month, names_to = "type", values_to = "quantity")

chart_1_labels <- tibble(
  month = ymd(20181001),
  quantity = c(292, 350),
  type = c("corrugated_paperboard_number", "corrugated_shipping_containers_number"),
  text = c("Corrugated Paperboard", "Corrugated Shipping Containers")
)

chart_1_comapre_points <- chart_1_data_longer %>% 
  filter(month %in% c(ymd(20200301), ymd(20210901)))

ggplot(chart_1_data_longer, aes(month, quantity, color = type)) +
  geom_vline(xintercept = ymd(20200301), color = "grey80", size = 0.8, linetype = "dashed") +
  geom_label(aes(x = ymd(20200301), y = 390, label = "Start of lockdown\nrestrictions"),
            nudge_x = 5, family = "Work Sans", size = 3.5, color = "grey60", hjust = 0,
            lineheight = 0.9, fill = "#F0F0F0", label.size = 0) +
  geom_line(size = 1.5) +
  geom_point(data = chart_1_comapre_points, size = 3) +
  geom_line(data = chart_1_comapre_points, size = 0.8, linetype = "dashed") +
  annotate("text", x = ymd(20210101), y = 375, label = "21% increase", family = "Work Sans",
           size = 3.5, angle = 25, fontface = "bold", color = "#457b9d") + 
  annotate("text", x = ymd(20200901), y = 290, label = "16% increase", family = "Work Sans",
           size = 3.5, angle = 15, fontface = "bold", color = "#a8dadc") + 
  geom_shadowtext(data = chart_1_labels, aes(label = text), hjust = 0, nudge_y = 1,
                  bg.color = "#F0F0F0", fontface = "bold", family = "Work Sans", size = 3.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y") +
  scale_y_continuous(limits = c(250, NA), expand = expansion(0), 
                     labels = c(" 250", " 300", " 350", "$400")) +
  scale_color_manual(values = c("#a8dadc", "#457b9d")) +
  coord_cartesian(clip = "off") +
  theme_fivethirtyeight(base_family = "Work Sans", base_size = 14) +
  labs(
    title = "Cardboard Prices in the U.S.",
    subtitle = "2019-2021"
  ) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.5)),
    plot.subtitle = element_text(face = "bold", color = "grey80", size = rel(1.2),
                                 margin = margin(b = 25)),
    legend.position = "none",
    axis.text.y = element_text(vjust = -0.5, margin = margin(r = -25))
  )
ggsave("plots/chart_1_cardboard_prices.png", width = 10, height = 6, dpi = 300)
