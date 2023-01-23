# prepare data for calculating monatery amount spent on books
spend_on_books <-
weights %>%
  rename(ar = timi) %>%
  left_join(income_data) %>%
  mutate(neysluupphaed_nominal = (vigt / 10000) * (radstofunartekjur * 10^6)) %>%
left_join(
books_and_prices %>%
  filter(undirvisitala == "Vísitala neysluverðs") %>%
  group_by(ar) %>%
  summarise(cpi = mean(cpi)))

spend_on_books <-
spend_on_books %>%
  group_by(undirvisitala) %>%
  mutate(last_cpi_value = last(cpi),
         cpi_real_p2019 = cpi / last_cpi_value) %>%
  mutate(neysluupphaed_nominal_p2019 = neysluupphaed_nominal / cpi_real_p2019)


# Chart 5 -- implied spend on books and spiel
spend_on_books %>%
  ggplot() + 
  aes(x = ar, y = neysluupphaed_nominal_p2019, color = undirvisitala) + 
  geom_line() + 
  scale_y_continuous(limits = c(0,60000), breaks = seq(0,60000, by = 10000), labels = scales::label_comma(big.mark = ".")) + 
  scale_x_continuous(limits = c(2002,2019), breaks = seq(2002,2019, by = 1)) + 
  theme_minimal() + 
  theme_eikonomics(axis_text = 14,legend_text = 10, axis_title = 16, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ylab("Útgjöld (verðlag 2019)") +
  xlab("Ár")

ggsave(filename = here("3_output", "Chart 5 -- implied spend on books and spiel 2002-2019.png"),
       width = 15*1.25, height = 10*1.25, units = "cm", bg = "white")
