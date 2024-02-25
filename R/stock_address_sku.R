
library(tidyverse)
 
   ads_stk <- data.table::fread("dados/ads_meli_stock.csv") %>% 
     as_tibble() %>%  janitor::clean_names()
   
   
   # Impacto SKU per address
   
  bd_sku_per_ads <-  ads_stk %>% 
     filter( str_detect(fbm_address_id, "^MZ")) %>% 
     mutate( erro_tipo = if_else( sku >=5, "Tuplas >=5", "Tuplas <5")) %>% 
      count( warehouse_id,erro_tipo, wt = sku, name = "sku") %>% 
        count( warehouse_id,erro_tipo, wt = sku, name = "sku") %>% 
        pivot_wider( names_from = erro_tipo, values_from = sku, values_fill = 0) %>% 
     mutate( "total_tuplas >=5" =   `Tuplas >=5` / (`Tuplas <5` + `Tuplas >=5`)) %>% 
     select(1,4)
  
  ads_stk %>% 
    filter(warehouse_id=="MXCD02") %>% 
    count(sku) %>% 
    
  
  
   
   ads_stk %>% 
     filter( warehouse_id %in% c("BRSP02", "BRSP04", "BRBA01")) %>% 
     filter( warehouse_id %>% str_detect("^BR"),
             !warehouse_id %>% str_detect("TR")) %>% 
     count(warehouse_id, sku) %>% 
     group_by(warehouse_id) %>% 
     mutate(pct = n / sum(n)) %>% 
     ungroup()  %>% 
     mutate(  sku_n = forcats::as_factor(sku)) %>% 
     filter(sku <=7) %>% 
     ggplot(aes( sku_n, pct)) +
     geom_col( fill="blue") +
     geom_text( aes(label =scales::percent(pct, accuracy = 1) ),
                color = "white", size = 3,vjust = 1 ) +
     scale_y_continuous( labels = scales::percent) +
     facet_wrap(~warehouse_id, scales = "free_y")
   
   
   ads_stk %>% 
     filter( str_detect( warehouse_id, "^MX"),str_detect(fbm_address_id , "^MZ")) %>% 
     muta
     count(sku,warehouse_id)
   
   