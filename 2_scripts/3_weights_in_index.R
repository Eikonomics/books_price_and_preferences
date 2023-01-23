# PXWEB query 
pxweb_query_list <- 
  list("Tími"=c("*"), "Undirvísitala"=c("IS09313", "IS0951"))

# Download data 
weights <- 
  pxweb_get(url = "http://px.hagstofa.is/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/2_undirvisitolur/VIS01305.px",
            query = pxweb_query_list)

# Convert to data.frame 
weights <- as.data.frame(weights, column.name.type = "text", variable.value.type = "text")

weights <-
  weights %>%
  janitor::clean_names() %>%
    mutate(timi = stringr::str_sub(timi, -4,-1)) %>%
    mutate(timi = as.numeric(timi)) %>%
    filter(timi >= 2002,
           timi <= 2019) %>%
    rename(vigt = visitala_neysluverds) 


# Chart 4 -- Weights Spiel vs Books 2002-2019
weights %>%
  ggplot() + 
  aes(x = timi, y = vigt/10000, color = undirvisitala) + 
  geom_line() + 
  scale_y_continuous(limits = c(0,120/10000), breaks = seq(0,160/10000, by = 20/10000), labels = scales::percent) + 
  scale_x_continuous(limits = c(2002,2019), breaks = seq(2002,2019, by = 1)) + 
  theme_minimal() + 
  theme_eikonomics(axis_text = 14,legend_text = 10, axis_title = 16, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ylab("Vog í VNV") +
  xlab("Ár")

ggsave(filename = here("3_output", "Chart 4 -- Weights Spiel vs Books 2002-2019.png"),
       width = 15, height = 10, units = "cm", bg = "white")

# next steps
# income 
# ratings