library(tidyverse)
library(geofacet)
library(ggthemes)
library(ggtext)

# Imports chart -----------------------------------------------------------

port_data_imports <- read_csv("data/cardboard_imports_state_data.csv")

port_data_imports_longer <- 
  port_data_imports %>%
  rowwise() %>% 
  mutate(fill_rect = if_else(`2020` > `2019`, "#a8dadcff", "#e63946ff")) %>% 
  pivot_longer(cols = 2:7, names_to = "year", values_to = "imports") %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(us_state_grid1, by = c("state" = "name"))

my_grid <- filter(us_state_grid1, code != "DC")

plot <- 
  ggplot(port_data_imports_longer, aes(year, imports, group = state)) +
  geom_rect(aes(xmin = 2019, xmax = Inf, ymin = -Inf, ymax = Inf, 
                fill = fill_rect), alpha = 0.05) +
  geom_line(color = "#457b9dff", size = 1.1) +
  geom_point(
    data = filter(port_data_imports_longer, year %in% c(2015, 2020)),
    size = 2, color = "#457b9dff"
  ) +
  scale_y_continuous(expand = expansion(0.2)) +
  scale_x_continuous(expand = expansion(0.2)) +
  scale_fill_identity() +
  labs(
    title = "Has the pandemic <span style='color: #a8dadcff'>increased</span> or <span style='color: #e63946ff'>decreased</span> state wise import of cardboard?",
    subtitle = "2015-2020"
  ) +
  facet_geo(~code, grid = my_grid, label = "name", scales = "free_y") +
  theme_fivethirtyeight(base_size = 14, base_family = "Work Sans") +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_markdown(size = rel(1.8), hjust = 0.5),
    plot.subtitle = element_text(size = rel(1.3), color = "#457b9dff", face = "bold",
                                 margin = margin(b = 10), hjust = 0.5),
    legend.position = "none",
    axis.text = element_blank(),
    strip.text = element_text(size = rel(0.7), face = "bold", color = "grey45"),
    panel.grid.major = element_blank(),
    panel.border = element_rect(linetype = "solid", fill = NA, 
                                color = "#D2D2D2", size = 0.5)
  )
ggsave("plots/chart_3_map_imports.png", plot, width = 14, height = 7)

# Exports chart -----------------------------------------------------------

port_data_exports <- read_csv("data/cardboard_exports_state_data.csv")

port_data_exports_longer <- 
  port_data_exports %>%
  rowwise() %>% 
  mutate(fill_rect = if_else(`2020` > `2019`, "#a8dadcff", "#e63946ff")) %>% 
  pivot_longer(cols = 2:7, names_to = "year", values_to = "exports") %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(us_state_grid1, by = c("state" = "name"))

my_grid_exports <- filter(us_state_grid1, ! name %in% c("Montana", "Wyoming", "Hawaii", "District of Columbia"))

plot <- 
  ggplot(port_data_exports_longer, aes(year, exports, group = state)) +
  geom_rect(aes(xmin = 2019, xmax = Inf, ymin = -Inf, ymax = Inf, 
                fill = fill_rect), alpha = 0.05) +
  geom_line(color = "#457b9dff", size = 1.1) +
  geom_point(
    data = filter(port_data_exports_longer, year %in% c(2015, 2020)),
    size = 2, color = "#457b9dff"
  ) +
  scale_y_continuous(expand = expansion(0.2)) +
  scale_x_continuous(expand = expansion(0.2)) +
  scale_fill_identity() +
  labs(
    title = "Has the pandemic <span style='color: #a8dadcff'>increased</span> or <span style='color: #e63946ff'>decreased</span> state wise export of cardboard?",
    subtitle = "2015-2020"
  ) +
  facet_geo(~code, grid = my_grid_exports, label = "name", scales = "free_y") +
  theme_fivethirtyeight(base_size = 14, base_family = "Work Sans") +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_markdown(size = rel(1.8), hjust = 0.5),
    plot.subtitle = element_text(size = rel(1.3), color = "#457b9dff", face = "bold",
                                 margin = margin(b = 10), hjust = 0.5),
    legend.position = "none",
    axis.text = element_blank(),
    strip.text = element_text(size = rel(0.7), face = "bold", color = "grey45"),
    panel.grid.major = element_blank(),
    panel.border = element_rect(linetype = "solid", fill = NA, 
                                color = "#D2D2D2", size = 0.5)
  )
ggsave("plots/chart_3_map_exports.png", plot, width = 14, height = 7)
