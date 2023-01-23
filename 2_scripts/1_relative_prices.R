# PXWEB query 
pxweb_query_list <- 
  list("Ár"=c("*"), "Mánuður"=c("*"), "Undirvísitala"=c("0","209", "191"))

# Download data 
books_and_prices <- 
  pxweb_get(url = "http://px.hagstofa.is/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/4_eldraefni/VIS01303.px",
            query = pxweb_query_list)

# Convert to data.frame 
books_and_prices <- as.data.frame(books_and_prices, column.name.type = "text", variable.value.type = "text")

books_and_prices <-
books_and_prices %>%
  janitor::clean_names() %>%
  group_by(undirvisitala) %>%
  mutate(date = seq.Date(from = as.Date("2002-01-01"), to = as.Date("2019-12-01"), by = "month")) %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(cpi = 100 * visitala_neysluverds/ visitala_neysluverds[1]) %>%
  mutate(ar = as.numeric(ar))

# Chart 1 -- CPI vs Books 2002-2019
books_and_prices %>%
  filter(undirvisitala != "09313 Leikjatölvur og tölvuleikir") %>%
  ggplot() + 
  aes(x = date, y = round(cpi,0), color = undirvisitala) + 
  geom_line() + 
  scale_y_continuous(limits = c(80,220), breaks = seq(80,220, by = 20)) + 
  scale_x_date(limits = c(as.Date("2002-01-01"), as.Date("2019-12-01")),date_breaks = "year", date_labels = "%Y") + 
  theme_minimal() + 
  theme_eikonomics(axis_text = 14,legend_text = 10, axis_title = 16, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ylab("Vísitala") +
  xlab("Ár")

ggsave(filename = here("3_output", "Chart 1 -- CPI vs Books 2002-2019.png"),
       width = 15, height = 10, units = "cm", bg = "white")

# Chart 3 -- Computerspiel und Machine vs Books 2002-2019
books_and_prices %>%
  filter(undirvisitala != "Vísitala neysluverðs") %>%
  ggplot() + 
  aes(x = date, y = round(cpi,0), color = undirvisitala) + 
  geom_line() + 
  scale_y_continuous(limits = c(20,220), breaks = seq(20,220, by = 20)) + 
  scale_x_date(limits = c(as.Date("2002-01-01"), as.Date("2019-12-01")),date_breaks = "year", date_labels = "%Y") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme_eikonomics(axis_text = 14,legend_text = 10, axis_title = 16, legend_position = "bottom") + 
  ylab("Vísitala") +
  xlab("Ár")

ggsave(filename = here("3_output", "Chart 3 -- Computerspiel und Machine vs Books 2002-2019.png"),
       width = 15, height = 10, units = "cm", bg = "white")

