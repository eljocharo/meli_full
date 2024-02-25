

   library(tidyverse)

   bd <- data.table::fread("../Documents/dados/BRSP04-MOVEMENTS_REPORT_28_jan_03_fev.csv") %>% 
       janitor::clean_names() %>% as_tibble()
   
   bd %>% glimpse()
   
   bd %>% 
     group_by(proceso) %>% 
     summarise( pecas  = sum(cantidad),
                itens = n_distinct(codigo_ml)) %>% 
     arrange(desc(pecas)) %>% gt::gt()

   bd_etl <- bd   %>%   
       mutate( area_origem  = str_sub(origen, 1,2),
                      area_destino  = str_sub(destino, 1,2),
               qty  = cantidad,
               fecha = lubridate::dmy_hms(fecha),
               data = fecha %>% as.Date()
               )
   
   
   
   bd_etl %>%  glimpse()
   
   bd_etl %>% 
       filter( area_destino == "",
               area_origem == "PW") %>% 
       group_by(origen) %>% 
   summarise( qtde = sum(qty),
              dat = n_distinct(data)) 
   
   # Analise Movimentos
  base_etl <-  bd_etl %>% 
     # filter( str_detect(proceso, "Put away|Picking")) %>% 
     select(proceso, "mov" = origen) %>% 
     bind_rows(
   bd_etl %>% 
     # filter( str_detect(proceso, "Put away|Picking")) %>% 
     select(proceso, "mov" = destino)
     ) %>% 
     mutate( proceso = str_extract(proceso, pattern="^[^\\s]+"),
              nivel  = str_sub(mov, 14,15),
             area  = str_sub(mov, 1,2))
  
  anl <- base_etl %>% 
    filter( area == "MZ") %>% 
    count(nivel) 
 
   anl %>% 
    mutate( pct = n / sum(n))
  
   
  mv <-  bd_etl %>% 
    filter( data == "2024-01-30") %>% 
     filter( area_origem == "MZ") %>% 
     group_by(area_origem, origen, proceso) %>% 
     summarise( qtde = sum(qty),
                dat = n_distinct(codigo_ml),
                n = n()) %>% 
    ungroup() %>% 
    mutate( mz  = str_sub(origen, 4,4),
            nivel  = str_sub(origen, 14,15) )
  
  
  bd_etl %>% 
        filter( proceso == "Cycle count") %>% 
   group_by(data) %>% 
    summarise( sku = n_distinct(codigo_ml),
               pessoas = n_distinct(responsable))
  
  
  mv %>% 
    count( nivel,proceso, wt = n) %>% 
    ggplot(aes(nivel, n)) +
    geom_col(fill = "blue") +
    geom_text(aes(label= n), size  =3, color = "white", vjust = 1) +
    facet_wrap( ~proceso, scales = "free_y") +
    scale_y_continuous( labels = scales::number) +
    labs(title = "Movimentos BRSP02 - 1 semana")
   
  inbound ##### 
  
  mv_inbound <- bd_etl %>% 
    filter( area_origem == "MU",
            proceso == "Put away") %>% 
    group_by( area_destino, destino, proceso) %>% 
    summarise( qtde = sum(qty),
               sku = n_distinct(codigo_ml),
               n = n()) %>% 
    ungroup() %>% 
    mutate( mz  = str_sub(destino, 4,4),
            nivel  = str_sub(destino, 14,15) ) 
  
  mv_inbound %>% 
    filter( area_destino !="") %>% 
  count( nivel, wt = sku, name = "sku" )  %>% 
  ggplot(aes(nivel, sku)) +
  geom_col(fill = "blue") +
  geom_text(aes(label= sku), 
            size  =6, 
            color = "white",
            vjust = 1) +
    labs( title = "Putaway") 
  
  
  
  bd_etl %>% 
    filter(proceso =="Cycle count") %>% view
  
  
  
  bd_etl %>%
  
    
    

  
  
 