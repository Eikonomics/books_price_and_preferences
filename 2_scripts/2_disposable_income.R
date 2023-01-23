# PXWEB query 
pxweb_query_list <- 
  list("Tekjur og skattar"=c("5"),
       "Eining"=c("0"),
       "Kyn"=c("0"),
       "Aldur"=c("0"),
       "Ár"= as.character(2002:2019))

# Download data 
income_data <- 
  pxweb_get(url = "http://px.hagstofa.is/pxis/api/v1/is/Samfelag/launogtekjur/3_tekjur/1_tekjur_skattframtol/TEK01001.px",
            query = pxweb_query_list)

pxweb_data_comments(income_data)

# Convert to data.frame 
income_data <- as.data.frame(income_data, column.name.type = "text", variable.value.type = "text")

income_data <-
  income_data %>%
  janitor::clean_names() %>%
  rename(radstofunartekjur = tekjur_eftir_kyni_og_aldri) %>%
  mutate(ar = as.numeric(ar)) %>%
  mutate(radstofunartekjur = radstofunartekjur/1000) %>%
  filter(ar >= 2002) %>%
  select(ar, radstofunartekjur)


# Chart 2 -- nominal incomes 2002-2019
income_data %>%
  ggplot() + 
  aes(x = ar, y = radstofunartekjur) + 
  geom_line() + 
  scale_y_continuous(limits = c(1.5,5.5), breaks = seq(1.5,5.5, by = 0.5)) + 
  scale_x_continuous(limits = c(2002,2019), breaks = seq(2002,2019, by = 1)) + 
  theme_minimal() + 
  theme_eikonomics(axis_text = 14,legend_text = 10, axis_title = 16, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ylab("Ráðstöfunartekjur (m. kr.)") +
  xlab("Ár")

ggsave(filename = here("3_output", "Chart 2 -- nominal incomes 2002-2019.png"),
       width = 15, height = 10, units = "cm", bg = "white")

