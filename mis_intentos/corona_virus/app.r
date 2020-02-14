data_conf <-   data_confirmados %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  pivot_longer(-c(1:4), names_to = "Fecha", values_to = "Confirmados") %>% 
  separate(Fecha, c("dia", "hora"),sep = " " ) %>% 
  select(-hora) %>% 
  group_by(`Province/State`, `Country/Region` ,  Lat,  Long, dia) %>% 
  summarise(Confirmados = max(Confirmados)) %>% 
  ungroup() %>% 
  mutate(dia = mdy(dia)) %>% 
  arrange(dia) %>% 
  mutate(dia = as.character(dia))

dias <- data_conf %>% distinct(dia)

data_conf <- data_conf %>% 
  mutate(Confirmados = ifelse(Confirmados <1, NA, Confirmados)) %>% 
  pivot_wider(names_from = dia, values_from = Confirmados) %>% 
  group_by( `Province/State`,`Country/Region` ,Lat, Long) %>% 
  nest() %>% 
  mutate(sequence = map(data, as.double)) %>% 
  rename(State = `Province/State`, Country= `Country/Region` ,
         lat = Lat,
         lon = Long) %>% 
  select(State, Country, lat, lon, sequence) %>% 
  mutate(grupo = "real")

ficticio <- tibble(
  `Province/State` = "ficticio",
  name  = "ficticio"       ,
  lat  = -2000,
  lon = -200000,
  sequence = list(rep(max(data_confirmados[[ncol(data_confirmados)]]),nrow(dias))),
  grupo = "ficticio"
)