library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggtext)

index_data <- read_csv("data/bls_index_data.csv")

date_range <- c(ymd(20190101), ymd(20210901))

colors <- c("Food & beverages carryout boxes" = "#1d3557ff",
            "Paper and allied products" = "#457b9dff",
            "All other end uses" = "#a8dadcff")

comment_1 <- str_wrap("For the first ten months or so of the pandemic, cardboard
                      prices have remained fairly level", width = 30)

# comment_2 <- str_wrap("Prices have sryrocketed post Dec-20 
#                       as a result of increased demand and reduced supply", 
#                       width = 35) %>% 
#   str_replace_all("\\n", "<br>")

comment_2 <- "Prices have sryrocketed post Dec-20<br> as a result of **<span style='color: #e63946ff'>increased demand</span>** and<br>**<span style='color: #e63946ff'>reduced supply</span>**"

labels_table <- tibble(
  date = ymd(20190101),
  index_value = c(313, 278, 367),
  type = c("Food & beverages carryout boxes", "All other end uses", "Paper and allied products")
)

index_data %>% 
  mutate(
    date = ymd(date, truncated = 1),
    type = if_else(type == "Metal and electrical machinery", "All other end uses", type)
  ) %>% 
  group_by(date, type) %>% 
  summarise(index_value = mean(index_value)) %>% 
  # filter(type != "Metal and electrical machinery")  
  ggplot(aes(date, index_value, color = type)) +
  annotate("rect", xmin = ymd(20200301), xmax = ymd(20201201), ymin = -Inf, ymax = Inf,
            color = "grey80", alpha = 0.1, size = 0) +
  geom_vline(xintercept = ymd(20200301), color = "grey80", size = 0.8, linetype = "dashed") +
  geom_line(size = 1.2) +
  geom_text(
    data = labels_table,
    aes(label = type), size = 3, fontface = "bold", hjust = 0
  ) +
  annotate("label", x = ymd(20200301), y = 410, label = "Start of lockdown\nrestrictions",
           family = "Work Sans", size = 3, color = "grey60", hjust = 0, 
           lineheight = 0.9, fill = NA, label.size = 0) +
  annotate("text", x = ymd(20200715), y = 384, 
           label = comment_1, family = "Work Sans", size = 3, color = "black",
           hjust = 0.5, lineheight = 0.9) +
  annotate("segment", x = ymd(20200301), xend = ymd(20201201), y = 370, yend = 370,
           size = 0.5, arrow = arrow(ends = "both", type = "closed", length = unit(1.5, "mm"))) +
  annotate("richtext", x = ymd(20201215), y = 330, 
           label = comment_2, family = "Work Sans", size = 3, color = "black",
           hjust = 0, lineheight = 1.1, fill = NA, label.size = NA) +
  annotate("segment", x = ymd(20201201), xend = ymd(20210701), y = 305, yend = 325,
           size = 0.5, arrow = arrow(ends = "last", type = "closed", length = unit(1.5, "mm"))) +
  scale_x_date(date_breaks = "3 months", 
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(250, 415), expand = expansion(0)) +
  scale_color_manual(values = colors) +
  labs(
    title = "Producer Price Index (PPI) for various types of\ncardboard containers",
    subtitle = "2019-2021"
  ) +
  coord_cartesian(clip = "off") +
  theme_fivethirtyeight(base_size = 12, base_family = "Work Sans") +
  theme(
    plot.margin = margin(10, 15, 10, 15),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.4)),
    plot.subtitle = element_text(face = "bold", color = "grey80", size = rel(1),
                                 margin = margin(b = 10)),
    legend.position = "none",
    axis.text = element_text(color = "grey40"),
    axis.text.y = element_text(vjust = -0.5, margin = margin(r = -20))
  )
ggsave("plots/chart_4_bls_cardboard_index_draft_02.png", width = 8, height = 5)
