spreadsheet_details <- gs4_get("1wszXLd_WTDRzjNdOAbsCjPJDCmrfVgol-Vb7QaU2D9U")

port_data <- read_sheet(
  ss = spreadsheet_details,
  sheet = "Cardboard imports - report",
  range = "A8:F520",
  col_names = c("Port", 2016:2020)
)

port_data %>% 
  filter(Port != "Total All Ports") %>% 
  extract(col = Port, into = c("port", "state", "type"), 
          regex = "([a-zA-Z]*,) ([a-zA-Z]*) (.*)") %>% 
  rowwise() %>% 
  mutate(total = sum(!is.na(c_across(4:8)))) %>% 
  filter(total == 5, type == "(Port)") %>% 
  arrange(state)
