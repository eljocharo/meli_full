 
  library(tidyverse)

  setwd("C:/Users/jorsouza/Documents/")
  
  inac <-  read_csv("dados/conteos_2024-02-23.csv")  
  inac %>% glimpse()
  
  area <- inac %>% 
    distinct(area)
  
  inac %>% 
    count(level)
  

# Status  INAC ------------------------------------------------------------
  inac %>%  
    select(cycle_count_id, warehouse_id, 
    region,"status" =  SHP_CC_CYCLE_COUNT_STATUS ) %>% 
    distinct() %>% 
    filter(status == "working")

# INAC por região ---------------------------------------------------------
  # Exemplo para calcular INAC por região E CAD
  
  inac %>% 
     filter(region =="MLM", mes =="2024 | 01", area =="MZ") %>% 
    select(region,warehouse_id,area,TARGET_SITE, population_size, p_hat ) %>% 
    mutate( 
            acuracidade = 1 - p_hat,
            acuracidade_tuplas = acuracidade * population_size  ) %>% 
    filter(!acuracidade_tuplas %>%  is.na()) %>% 
    group_by(warehouse_id, region) %>% 
    summarise( target_site = max(TARGET_SITE),
              population_size_tuplas = sum(population_size),
               acuracidade_tuplas = sum(acuracidade_tuplas)) %>% 
    ungroup() %>% 
    group_by(warehouse_id) %>% 
    summarise(population_size_tuplas = sum(population_size_tuplas, na.rm = T),
              acuracidade_tuplas = sum(acuracidade_tuplas, na.rm = T), .groups = "drop",
              target_site = max(target_site)) %>% 
    ungroup() %>% 
    mutate( inac_region = acuracidade_tuplas / population_size_tuplas) %>%
    mutate( goal = if_else(target_site <=inac_region, "OK", "NotOK")) %>% 
    mutate( pesoCad = population_size_tuplas / sum(population_size_tuplas)) %>% 
    
    add_row(warehouse_id = "MLB", population_size_tuplas = sum(.$population_size_tuplas),
            acuracidade_tuplas = sum(.$acuracidade_tuplas),
            inac_region = sum(.$acuracidade_tuplas) / sum(.$population_size_tuplas),
            target_site = max(inac$TARGET_REGION),
            ) %>% 
    arrange(desc(population_size_tuplas)) %>% 
  
    gt::gt() %>% 
    gt::fmt_number(columns = c(2,3), locale = "pt_BR", decimals = 0) %>% 
    gt::fmt_percent(columns = c(4,5,6, 7)) %>% 
    gt::tab_header( title = "INAC - MLB") %>% 
    gt::tab_footnote(footnote = "INAC feb.2024")
  
# Tabela IMPACTO NO INAC CAD VS AREA------------------------------------------------
  
  pais <- "MLB"
  
  gf_impacto_cd_vs_area <-  inac %>%
         filter(region == pais, mes =="2024 | 02") %>%
    # Calculo por CD
      mutate( tuplas_erros_proj = p_hat * population_size) %>% 
      add_count(warehouse_id , wt = acuracidade, name = "acuracidade_wh") %>% 
      add_count(warehouse_id , wt = population_size, name = "population_size_wh") %>% 
      mutate(inac_site = acuracidade_wh / population_size_wh) %>% 
    # Calculo por Area
    add_count(area,         wt = tuplas_erros_proj, name = "erro_tuplas_area") %>% 
    add_count(warehouse_id, wt = tuplas_erros_proj, name = "erro_tuplas_wh") %>% 
    add_count(              wt = tuplas_erros_proj, name = "erro_total") %>% 
    mutate( pct_area = ( erro_tuplas_area  / erro_total),
            pct_area =  scales::percent(pct_area, accuracy=0.1)) %>%  
    # Calculo por Regiao
    add_count( region, wt = acuracidade, name = "total_acuracidade_regiao") %>% 
    add_count( region, wt = population_size, name = "total_population_regiao") %>% 
    mutate( pct_regiao = ( total_acuracidade_regiao  / total_population_regiao),
            pct_regiao =  scales::percent(pct_regiao, accuracy=0.1),
            target_region = scales::percent(TARGET_REGION, accuracy=0.1)) %>% 
    
    mutate( pct_wh = (erro_tuplas_wh  / erro_total),
            pct_wh_ =  scales::percent(pct_wh, accuracy=0.1),
            inac_site_ = scales::percent(inac_site, accuracy=0.01),
            target_site_ = scales::percent(TARGET_SITE , accuracy=0.01),
            Goal = if_else( inac_site >= TARGET_SITE, true = "*****", false = "") ,
            warehouse_id = str_c(str_c( warehouse_id, "    (", pct_wh_, ")","\n", "Target ", target_site_, "\n",Goal, " INAC ", inac_site_ ) ) 
             ) %>% 
    mutate( area =  str_c( area, "(", pct_area, ")" ), 
           area = fct_reorder(area, erro_tuplas_area, sum, .desc = T),
           inac_area_impact = acuracidade / acuracidade_wh,
           inac_area_impact_pct = scales::percent(inac_area_impact, accuracy=0.1)) 
  

# Grafico Tabela de Impacto -----------------------------------------------

  gf_impacto_cd_vs_area %>% 
    ggplot(aes(x = area, y = reorder(warehouse_id, (pct_wh)), fill = inac_area_impact)) +
    geom_tile(color = "white") +
    geom_text(aes(label = scales::percent(inac_area_impact, accuracy = 0.1)),
              color = "white", size = 4) +
    scale_fill_gradientn(colors =  rev(hcl.colors(4, "RdYlGn")),
                          labels = scales::percent,
                         name = "error") +
    labs(title = str_c( "2024-feb ",pais, " Target ",gf_impacto_cd_vs_area[["target_region"]][1] ,
                        "  INAC ", gf_impacto_cd_vs_area[["pct_regiao"]][1]), y = "", x = "") +
    scale_x_discrete(position = "top") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10),  # Ajusta o tamanho do texto do eixo y
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 12),  # Ajusta o tamanho do texto do eixo x
          axis.title.x = element_text(size = 12),
          legend.position = c(.03, 1.05),
          legend.background =  element_blank(),
          # Move a legenda para o topo do gráfico
          legend.direction = "horizontal",
          legend.text = element_text(color = "black", size = 6, vjust = 25),  # Define a cor do texto da legenda para branco
          legend.title = element_text(color = "black", vjust = .9, hjust = 0.5, ),  # Define a cor do título da legenda para branco
          panel.background = element_rect(fill = "#2d2d2d"),   # Define a cor de fundo do painel para preto
          panel.grid.major = element_line(color = "#2d2d2d"),  # Define a cor das linhas de grade principais para cinza
          panel.grid.minor = element_line(color = "black"),  # Define a cor das linhas de grade menores para cinza, se necessário
          plot.title = element_text(color = "#2d2d2d", 
                                    hjust = .5, vjust = -2,
                                    size = "18"))  # Define a cor do título do gráfico para branco)  # Define a direção da legenda como horizontal) 
   
# Tabela com Erros Tuplas por area ----------------------------------------
  
  inac %>% 
    filter(mes == "2024 | 02", !area == "MZ") %>% 
    select(warehouse_id:population_size, acuracidade, 
           tuplas_contadas_error,region, region, TARGET_REGION, area) %>% 
    tidyr::replace_na( list( acuracidade = 0,  area = "NA")) %>% 
    count( region, area,  wt = tuplas_contadas_error) %>% 
    pivot_wider( names_from = region, values_from = n) %>% 
     tidyr::replace_na( list( `Cono Sur` = 0,  MLB  = 0, MLM  = 0)) %>% 
    group_by(area) %>% 
    mutate( total_error = sum(`Cono Sur`+ MLM +MLB)) %>% 
    ungroup() %>% 
    arrange(desc(total_error)) %>% 
    mutate( pct = total_error / sum(total_error)) %>% 
    
    # Adicionar linha de total
    bind_rows(summarise(., across(where(is.numeric), sum, na.rm = TRUE), 
                        area = "Total", pct = sum(total_error) / sum(total_error))) %>%
    # arrange(desc(area)) %>%  # Organizar para manter a linha Total no final se necessário
   gt::gt() %>% 
    gt::tab_header(title = "INAC - Febrero - Errores Tuplas") %>% 
    gt::fmt_percent(columns = pct, ) 
  
  
  
  
    
# INVENTÁRIO ADDRESS -DETAIL ----------------------------------------------

  inv_address <-  data.table::fread("dados/conteos_detail_2024-02-23_v2.csv") %>% 
    janitor::clean_names() %>% dplyr::as_tibble() %>% 
    mutate(pais = case_when( 
      str_sub(  warehouse_id, 1,2 ) == "BR" ~"MLB",
      str_sub(  warehouse_id, 1,2 ) == "MX" ~"MLM",
      TRUE ~ "ConoSur" ) ) %>% 
    mutate( div = count_unit_ok -qty_system ,
            div_type = if_else( 
              div ==0, "ok",
              false = if_else(
                div >0, "Found", "Lost" 
              ) ) ) %>% 
    mutate(data_criacao = lubridate::ymd(data_criacao),
           mes = lubridate::month(data_criacao, abbr = T, label = T)) %>% 
    dplyr::relocate(pais, .before = warehouse_id)
  

  inv_address %>% 
   count(area, mes) %>% 
    pivot_wider( names_from = mes, values_from = n)
    
   

# Grafico por tipo de Erro Lost e Found ( endereço) -----------------------
  

  inv_address %>% 
    filter(pais== "MLB", mes == 'fev', div_type != "ok") %>% 
      group_by(warehouse_id,level, div_type) %>% 
    summarise( n = n()) %>% 
    mutate( pct = n /sum(n)) %>% 
    ggplot(aes(n, as.character(level), fill = div_type)) +
    geom_bar(position = "fill", stat = "identity", color="black", alpha=.8) +
    scale_fill_manual(values = c("#3c64f7", "#c40000"), name = "") +
    scale_x_continuous( labels = scales::percent) +
    geom_vline(xintercept =  c(0.5), 
               linetype = "dashed", color = "black") +
    labs( title = "Cono Sur Divergencia Type - Inv. Mostreo Febrero",
          x = " Level address", y="") +
    geom_text(aes(label = scales::percent(pct, accuracy = 1), 
                  x = n,
                  y = as.character(level)), 
              position = position_fill(vjust = .4), 
              color = "black", 
              size = 3, 
              hjust = 0.5) +
    facet_wrap(~warehouse_id) +
    tidyquant::theme_tq() +
    theme( legend.position =  "top",
           legend.title = element_blank())
  
  

# Impacto em volume de tuplas Lost e Found -------------------------------------------------------
  
  inv_address %>% 
    filter(pais == "MLM",
           area== "MZ",
           warehouse_id == "MXJC01",
           div_type != "ok",
            mes == "jan") %>% 
    count(warehouse_id, level, div_type) %>% 
    add_count(warehouse_id, wt = n, name = "tot") %>%
    mutate(warehouse_id = str_c(warehouse_id, " (Tuplas error ", tot, ")")) %>% 
    mutate(warehouse_id = fct_reorder(warehouse_id, -n, .fun = sum)) %>% 
    mutate(div_type = factor(div_type, levels = c("Lost", "Found"))) %>% 
    ggplot(aes(x = level, y = n, fill = div_type)) +
    geom_col(color = "black", alpha = 0.8, position = position_dodge(width = 0.5)) +
    geom_text(aes(label = n, color = div_type), 
              position = position_dodge(width = 0.5), vjust = -0.5, size = 3) +
     scale_x_continuous(breaks = 1:7) +
    # scale_y_continuous(breaks = seq(0, 1900, by = 200),
                       # limits = c(0, 1900))+
     scale_fill_manual(values = c("Found" = "#3c64f7", "Lost" = "#c40000"), name = "") +
     scale_color_manual(values = c("Found" = "#3c64f7", "Lost" = "#c40000")) +
     guides(color = FALSE) +
    labs(#title = "MLM Type error - Conteo Mostreo Febrero",
         x = "Level address", y = "Tuplas con error") +
    facet_wrap(~warehouse_id, scales = "free_y") +
    tidyquant::theme_tq() +
    theme(legend.position = "top",
          legend.title = element_blank())
  
    
  # erro geral
  inv_address %>% 
    filter( divergencia_tt != 0) %>% 
    summarise( ads = n_distinct(shp_warehouse_address),
               tuplas = n_distinct(shp_inventory_id),
               .by = c( "pais" ) ) %>% 
    arrange(pais, desc(tuplas) ) %>% 
    mutate( pct_geral = tuplas / sum(tuplas) *100) 
  
  # Preparando e gerando o grafico de frquencia em erros tuplas -------------
  
  inv_address %>%
    filter( str_detect( shp_warehouse_address, "^MZ")) %>% 
    mutate( erro_tuplas = if_else( divergencia_tt != 0, "sim", "não")) %>% 
    group_by(pais, warehouse_id, shp_warehouse_address, erro_tuplas) %>%
    summarise(sku = n_distinct(shp_inventory_id), .groups = "drop") %>%
    pivot_wider( names_from = erro_tuplas,
                 values_from = sku,
                 values_fill = 0) %>% 
    mutate( total_sku = não + sim ) %>% 
    count(pais, warehouse_id, total_sku, wt = sim, name = "sku") %>% 
    group_by(pais, warehouse_id) %>%
      slice(1:7) %>% 
    mutate(pct_sku = sku / sum(sku),
           sku_per_ads = as.character(total_sku  )) %>%
    ungroup() %>%
    add_count(pais, warehouse_id, wt = sku) %>%
    mutate(warehouse_id = str_c(warehouse_id, " (Tuplas error ", n, ")")) %>% 
    mutate(warehouse_id = fct_reorder(warehouse_id, -sku, .fun = sum)) %>%
    group_by(pais, warehouse_id) %>% 
    mutate(F = ifelse(rank(-sku) <= 2, TRUE, FALSE)) %>%
    ggplot(aes(sku_per_ads, sku, fill = factor(F)) ) +
    geom_col(data = . %>% filter(F == TRUE), fill = "#c40000", color = "black", alpha = .7 ) +
    geom_col(data = . %>% filter(F == FALSE), fill = "#e6e6e6", color = "black", alpha = .9 ) +
    geom_text( data = . %>% filter(F == FALSE),
               aes(label = sku), size = 3, color = "black", vjust = 1) +
    geom_text( data = . %>% filter(F == TRUE),
               aes(label = sku), size = 4, color = "white", vjust = 1) +
    facet_wrap(~warehouse_id, scales = "free_y") +
    labs(title = "MLB - Conteos mostreo Feb.2024",
         subtitle = "Errores tuplas y niveles",
         y = "tuplas con errores",
         x = "SKU per address",
         fill = "F") +
    theme(axis.text.x = element_text(size = 12)) +
    scale_y_continuous(labels = scales::number) +
    tidyquant::theme_tq() 
  
  
  

# Medição Impacto acima de 5 SKU ------------------------------------------
  
  
  #  Total de Tuplas
  tutplas_total <- inv_address %>%
    filter( str_detect( shp_warehouse_address, "^MZ")) %>% 
    mutate( erro_tuplas = if_else( divergencia_tt != 0, "sim", "não")) %>% 
    group_by(pais, warehouse_id, shp_warehouse_address, erro_tuplas) %>%
    summarise(sku = n_distinct(shp_inventory_id), .groups = "drop") %>%
    pivot_wider( names_from = erro_tuplas,
                 values_from = sku,
                 values_fill = 0) %>% 
    mutate( total_sku = não + sim ) %>% 
    mutate( erro_tipo = if_else( total_sku >=5, "Tuplas >=5", "Tuplas <5")) %>% 
    count( pais, warehouse_id, erro_tipo, wt = total_sku) %>% 
    pivot_wider( names_from = erro_tipo, values_from = n, values_fill = 0) %>% 
    mutate( "total_tuplas >=5" =   `Tuplas >=5` / (`Tuplas <5` + `Tuplas >=5`))
  

  inv_address %>%
    filter( str_detect( shp_warehouse_address, "^MZ")) %>% 
    mutate( erro_tuplas = if_else( divergencia_tt != 0, "sim", "não")) %>% 
    group_by(pais, warehouse_id, shp_warehouse_address, erro_tuplas) %>%
    summarise(sku = n_distinct(shp_inventory_id), .groups = "drop") %>%
    pivot_wider( names_from = erro_tuplas, values_from = sku, values_fill = 0) %>% 
    mutate( total_sku = não + sim ) %>% 
    count(pais, warehouse_id, total_sku, wt = sim, name = "sku") %>% 
    mutate( erro_tipo = if_else( total_sku >=5, "Tuplas >=5", "Tuplas <5")) %>% 
    count(pais, warehouse_id, erro_tipo, wt = sku, name = "sku") %>% 
    group_by( pais, warehouse_id) %>% 
    mutate(pct = sku / sum(sku)) %>%  ungroup() %>% 
    add_count(warehouse_id, wt = sku, name = "total_sku") %>% 
    left_join( bd_sku_per_ads, by = join_by(warehouse_id)) %>% 
    mutate(warehouse_id = str_c(warehouse_id, " (Tuplas error ", total_sku, ")", "\n",
    "Address stock >= 5 tuplas ",  scales::percent(`total_tuplas >=5`, accuracy=1 ) )) %>% 
    mutate(warehouse_id = fct_reorder(warehouse_id, -sku, .fun = sum)) %>% 
    mutate( label_sku = str_c(sku, "\n"," ",scales::percent(pct, accuracy=1 )) ) %>% 
     ggplot(aes(erro_tipo , sku, fill = factor(F)) ) +
     geom_col(fill = "#f7e13c", color = "black", alpha = .9 ) +
    geom_text(aes(label = label_sku), size = 4, color = "#283d4e", vjust = 1) +
    facet_wrap(~warehouse_id, scales = "free_y") +
    labs(title = "Area MZ | Conteos mostreo Feb.2024",
         subtitle = "Errores tuplas per address",
         y = "tuplas con errores",
         x = "Tuplas per address",
         fill = "F") +
    theme(axis.text.x = element_text(size = 12)) +
    scale_y_continuous(labels = scales::number) +
    tidyquant::theme_tq() 
  
  
  # Preparando dados para gerar a tabela de contingencia --------------------
  
  inv_anl <-  inv_address %>% 
    filter( divergencia_tt != 0,
            level<=7, area == "MZ",pais=="ConoSur" ) %>% 
    group_by(pais, warehouse_id, shp_warehouse_address, level) %>% 
    summarise( sku = n_distinct(shp_inventory_id), 
               .groups = "drop") %>% 
    group_by(pais, warehouse_id, level) %>% 
    summarise(sku = sum(sku) ) %>% 
    mutate( pct_sku = sku / sum(sku)) %>% 
    ungroup()
  
  # Tabela de % contigencia erros de tuplas por nivel -----------------------
  
  inv_anl %>% 
    ggplot(aes(x = level %>% as.character(), y = reorder(warehouse_id, (sku)), fill = pct_sku)) +
    geom_tile(color = "white") +
    geom_text(aes(label = scales::percent(pct_sku, accuracy = 0.1)),
              color = "black", size = 4) +
    scale_fill_gradientn(colors =  rev(hcl.colors(5, "RdYlGn")), #c("#ffffff","#00aa7f","#00aa00" ,"#f7e13c","#005500"),
                         labels = scales::percent,
                         name = "error") +
    labs(title = str_c( "2024-feb " ,"INAC Error"),
         y = "", x = "Level") +
    scale_x_discrete(position = "top") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10),  # Ajusta o tamanho do texto do eixo y
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 12),  # Ajusta o tamanho do texto do eixo x
          axis.title.x = element_text(size = 12),
          legend.position = c(.04, 1.07),
          legend.background =  element_blank(),
          # Move a legenda para o topo do gráfico
          legend.direction = "horizontal",
          legend.text = element_text(color = "black", size = 6, vjust = 25),  # Define a cor do texto da legenda para branco
          legend.title = element_text(color = "black", vjust = .9, hjust = 0.5, ),  # Define a cor do título da legenda para branco
          panel.background = element_rect(fill = "#2d2d2d"),   # Define a cor de fundo do painel para preto
          panel.grid.major = element_line(color = "#2d2d2d"),  # Define a cor das linhas de grade principais para cinza
          panel.grid.minor = element_line(color = "black"),  # Define a cor das linhas de grade menores para cinza, se necessário
          plot.title = element_text(color = "#2d2d2d", 
                                    hjust = .5, vjust = -2,
                                    size = 10))  # Define a cor do título do gráfico para branco)  # Define a direção da legenda como horizontal) 
  
  
  # Erros de Tuplas por SKU no endereço -------------------------------------
  
  base_sku_erro <-  inv_address %>% 
    mutate( erro = if_else( divergencia_tt != 0, "Erro", "OK")) %>% 
    select(pais,warehouse_id,shp_warehouse_address, erro, shp_inventory_id) %>% 
    group_by(pais,warehouse_id,shp_warehouse_address, erro) %>% 
    summarise( sku_ads = n_distinct(shp_inventory_id)) %>% 
    ungroup() %>% 
    pivot_wider( names_from = erro,
                 values_from = sku_ads, values_fill = 0) %>% 
    mutate(total_tuplas = OK+Erro)
  
  base_sku_erro %>% 
    filter(str_detect(shp_warehouse_address, "^MZ"), pais == "MLM") %>% 
    group_by(pais,warehouse_id) %>% 
    mutate( erro = if_else(Erro != 0, "erro", "ok")) %>% 
    ungroup() %>% 
    count(pais,warehouse_id,erro, total_tuplas, wt = total_tuplas) %>% 
    pivot_wider( names_from = erro,
                 values_from = n,
                 values_fill = 0) %>% 
    mutate(inac =1- erro / (erro +ok)) %>% 
    group_by(pais, warehouse_id) %>% 
    slice(1:7) %>% 
    ungroup() %>% 
    
    ggplot(aes(x = total_tuplas , y =  erro)) +
    geom_col( fill = "#f7e13c", color = "black", alpha = 1 ) +
    geom_text( aes(label = erro), size = 3, color = "black", vjust = 1) +
    facet_wrap(~warehouse_id, scales = "free_y") +
    labs(title = "MLB - Conteos mostreo 2024",
         subtitle = "Errores tuplas y niveles",
         y = "tuplas con errores",
         x = "Qty Tuplas per address",
         fill = "F") +
    scale_x_continuous(breaks = seq(1, 7))+
    tidyquant::theme_tq()
  # Saber quantos tuplas contou por nivel -----------------------------------

  inv %>% 
    filter(pais == "MLM", area == "MZ") %>% 
    mutate(divergencia_tt = count_unit_ok - qty_system ) %>% 
    mutate(  div_type = if_else( 
      divergencia_tt ==0, "ok",
      false = if_else(
        divergencia_tt >0, "Found", "Lost" )
    ) ) %>% 
    group_by(warehouse_id, level, shp_warehouse_address, div_type) %>% 
    summarise(tuplas = n_distinct(shp_inventory_id), .groups = "drop") %>% 
    group_by(warehouse_id, level, div_type) %>% 
    summarise(tuplas = sum(tuplas), .groups = "drop") %>% 
    mutate(warehouse_id = fct_reorder(warehouse_id, -tuplas, .fun = sum)) %>% 
    ggplot(aes(level, tuplas, fill = factor(div_type))) +
    geom_col( color = "black", alpha = .95) +
    geom_text(aes(label = tuplas), size = 3, color = "black", vjust=1) +
    facet_wrap(~warehouse_id, scales = "free_y") +
    labs(title = "MLB - Conteos mostreo 2024",
         subtitle = "tuplas y niveles",
         x = "tuplas con errores",
         y = "niveles") +
    theme(axis.text.x = element_text(size = 12)) +
    scale_x_continuous(breaks = seq(0, 7)) +
    theme_tq()
  
  
  
 
  