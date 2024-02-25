

library(tidyverse)
library(psych)

  ads <- data.table::fread("dados/BRSP02-ADDRESS_REPORT-2024-02-18.csv") %>% 
    janitor::clean_names() %>%  as_tibble() %>% 
    mutate( warehouse = "sp02")
  
  ads_spquatro <- data.table::fread("dados/BRSP04-ADDRESS_REPORT-2024-02-18.csv") %>% 
    janitor::clean_names() %>%  as_tibble() %>% 
    mutate( warehouse = "sp04")
  
  
  stk <- data.table::fread("dados/bq-results-20240218-160503-1708272507577.csv") %>% 
    janitor::clean_names() %>%  as_tibble() 
  


# Base Unificada Analise --------------------------------------------------

  
  ads_etl <- ads %>% 
    bind_rows(
      ads_spquatro
    ) %>% 
     mutate( area = str_sub(address, start = 1,end =2),
              nivel_mz =  str_sub(address, start = 4,end =4),
             rua =  str_sub(address, start = 6,end =8),
             mod =  str_sub(address, start = 10,end =12),
             andar =  str_sub(address, start = 14,end =15))
  
  
  ads_etl %>% 
    count(warehouse, wt = cantidad_total)
  
  base_etl_ads <- ads_etl %>% 
    filter( area == "MZ") %>% 
    group_by(warehouse, address) %>% 
    summarise( pecas = sum(cantidad_total),
               sku = n_distinct(inventory_id)
              ) %>% 
    mutate( area = str_sub(address, start = 1,end =2),
            nivel_mz =  str_sub(address, start = 4,end =4),
            rua =  str_sub(address, start = 6,end =8),
            mod =  str_sub(address, start = 10,end =12),
            andar =  str_sub(address, start = 14,end =15) %>% as.numeric())
  
  base_etl_sku <- ads_etl %>% 
    filter( area == "MZ") %>% 
    group_by(warehouse,inventory_id) %>% 
    summarise(  pecas = sum(cantidad_total),
               ads_etl = n_distinct(address)
    )
  
  base_etl_ads %>% 
    group_by(warehouse) %>% 
    count(warehouse, andar, wt = pecas) %>% 
    mutate( pct = n / sum(n),
            pcc = cumsum(pct)) %>% 
    pivot_wider( names_from = andar, values_from = pct, c(-n, -pcc))
  
  
  #  Nivel Endereço
  base_etl_ads %>% 
    ggplot(aes( warehouse, sku)) +
    geom_boxplot() +
    scale_y_continuous(breaks = 1:7) +
    geom_hline(yintercept = c(3,4),  color = "blue")
  
  base_etl_ads %>% 
    filter( pecas <=20) %>% 
    ggplot(aes( warehouse, pecas)) +
    geom_boxplot() +
    scale_y_continuous(breaks = 1:15) +
    geom_hline(yintercept = c(6,7),  color = "blue")
  
  
  #  Nivel SKU
  base_etl_sku %>% 
    filter( pecas <=20) %>% 
    ggplot(aes( warehouse, pecas)) +
    geom_boxplot() +
    scale_y_continuous(breaks = 1:15) +
    geom_hline(yintercept = c(6,7),  color = "blue") +
    labs(title = "Unidades por SKU")
  
  base_etl_sku %>% 
   filter( ads_etl <=20) %>% 
    ggplot(aes( warehouse, ads_etl)) +
    geom_boxplot() +
    scale_y_continuous(breaks = 1:15) +
    geom_hline(yintercept = c(6,7),  color = "blue") +
      labs(title = "Endereços por SKU")
  
  
  base_etl_ads %>% 
    count(rua, wt =  sku  ) %>% 
     slice(1:10) %>% 
    mutate( pct = n / sum(n),
            pcc = cumsum(pct))
 
  
  
  ## Resumo geral
 
 fl <- area %>% pull()
 
 ads_etl %>% 
   filter( area %in%  fl) %>% 
   summarise( pecas = sum(cantidad_total),
              sku = n_distinct(inventory_id),
              enderecos = n_distinct(address))
 
 
 ver <- ads_etl %>% 
   filter( area %in%  fl) %>% 
   group_by(inventory_id) %>% 
   summarise(ads = n_distinct(address),  .groups = "drop")
 
 top_sku_espalhado <- ver %>% 
   top_n( 123) 
   
 
 ads_etl %>% 
   filter( inventory_id %in%  top_sku_espalhado$inventory_id ) %>% 
   count(area, sort = T)
   
 
 ver %>%  
   count(ads) %>% 
   mutate(pct = n / sum(n),
          pct_acc = cumsum(pct)) %>% 
   slice(11:n()) 
   
 
 
 base_tupla <- ads_etl %>% 
   select(inventory_id, address, area, ultima_actualizacion) %>% 
   mutate( update = lubridate::dmy_hms(ultima_actualizacion)) %>% 
   group_by(area, address) %>% 
   summarise( add = n_distinct(inventory_id),
              update = max(update), .groups = "drop")
 
 
 
 ads_etl %>% 
   filter( area %in%  fl) %>% 
   count( andar, wt = cantidad_total, sort = T) %>% 
   slice(1:5) %>% 
   mutate( n_pct = n / sum(n))
   
   ads_etl %>% 
   select(vendedor, valor_de_seguro, inventory_id, cantidad_total) %>% 
  summarise( media = mean(valor_de_seguro,  na.rm = T))
 
 ads_etl %>% 
   select(vendedor, valor_de_seguro, inventory_id, cantidad_total) %>% 
   top_frac(0.01, wt = valor_de_seguro) %>% 
   arrange((valor_de_seguro))
 
 
 
   
 
 # Tuplas em BRSP02 2799614 Inventario
 
 base_tupla %>% 
   top_n(50, add) %>% 
   arrange(desc(add)) %>% view
 
 ## Analise por NIVEL mz
 ads_etl %>% 
   filter( area == "MZ") %>% 
   group_by(nivel_mz) %>% 
   summarise( pecas = sum(cantidad_total),
              sku = n_distinct(inventory_id),
              enderecos = n_distinct(address)) %>% 
   arrange(desc(pecas))
 
 
 ## Resumo por area

 ads_etl %>% 
   group_by(area) %>% 
   summarise( pecas = sum(cantidad_total),
              sku = n_distinct(inventory_id),
              enderecos = n_distinct(address)) %>% 
   arrange(desc(pecas))
 
 ## Resumo por andar
 ads_etl %>% 
   filter( area != "MU") %>% 
   group_by(andar) %>% 
   summarise( pecas = sum(cantidad_total),
              sku = n_distinct(inventory_id),
              enderecos = n_distinct(address)) %>% 
   arrange(desc(pecas)) %>% 
   mutate( pctpecas = pecas / sum(pecas),
           pecas_acc = cumsum(pctpecas),
           pctsku = sku / sum(sku),
            sku_acc = cumsum(pctsku))
 
 
  ## Analise de endereços
 
 ads_resumo <- ads_etl %>%  
   filter( area == "MZ") %>% 
   select(address, cantidad_total, inventory_id, area) %>% 
   group_by(address, area) %>% 
   summarise( sku = n_distinct(inventory_id),
              pecas = sum(cantidad_total)) %>% 
   ungroup()
 
 
 ads_resumo <- bd %>%  
   filter(str_detect(address,"^MZ") ) %>% 
   select(warehouse, address, cantidad_total, inventory_id) %>% 
   group_by(warehouse,address) %>% 
   summarise( sku = n_distinct(inventory_id),
              pecas = sum(cantidad_total)) %>% 
   ungroup()
 
 
 
 ads_resumo %>% 
    
filter(warehouse=="sp02") %>% 
   summary()
 
 ads_resumo %>% 
   
   filter(warehouse=="sp04") %>% 
   summary()
  
 ads_resumo %>% 
   count(warehouse)

 ## Analise por andar no MZ
 ads_etl %>%  
   filter( area == "MZ") %>% 
   select(address, cantidad_total, inventory_id, area, andar, nivel_mz) %>% 
   group_by(andar) %>% 
   summarise( sku = n_distinct(inventory_id),
              pecas = sum(cantidad_total)) %>% 
   ungroup() %>% 
   mutate( pctpecas = pecas / sum(pecas),
           pecas_acc = cumsum(pctpecas),
           pctsku = sku / sum(sku),
           sku_acc = cumsum(pctsku))
 
## Analise por SKU
 
  sku_resumo <-  ads_etl %>%  
     filter( area == "MZ") %>% 
     select(address, cantidad_total, inventory_id, area, andar, nivel_mz) %>% 
     group_by(inventory_id) %>% 
     summarise( address = n_distinct(address),
                pecas = sum(cantidad_total)) %>% 
     ungroup()
  
  sku_resumo %>% 
    summary()
  
  sku_resumo %>% 
    count(address) %>% 
    slice(1:20) %>% 
    mutate( pct = n / sum(n),
          pct_acc = cumsum(pct))
  
  
  ## Analise de MU
  
  ads_etl %>%  
    filter( area == "MU") %>% 
    select(address, cantidad_total, inventory_id, area) %>% 
    group_by(address) %>% 
    summarise( sku = n_distinct(inventory_id),
               pecas = sum(cantidad_total)) %>% 
    ungroup() %>% 
    summary()
  

# Analise Query Stock -----------------------------------------------------


  
  ads_etl <-  stk %>% 
    mutate( area = str_sub(fbm_adddress_id, start = 1,end =2),
            nivel_mz =  str_sub(fbm_adddress_id, start = 4,end =4),
            andar =  str_sub(fbm_adddress_id, start = 14,end =15) %>% as.numeric()) 
  
  
  
  ads_etl %>% 
    group_by(warehouse_id) %>% 
    summarise( media = mean(qtde),
               n = n(),
               unit = sum(qtde),
               min = min(qtde),
               max = sum(qtde))
  
  
  ads_etl %>% 
    top_n(6, qtde)
  

# tuplas resumo -----------------------------------------------------------


 tuplas_base <-  ads_etl  %>% 
    filter( str_detect(address, "^MZ" )) %>% 
    group_by(warehouse, address) %>% 
    summarise( n_tuplas = n_distinct(inventory_id)) %>% 
    ungroup()
  
  
  tuplas_base %>% 
    count(warehouse, wt = n_tuplas, name = "tuplas")
    
  
  


   
             