# Numerando o cenário
dic <- data.frame(cenario = unique(df$cenario), cenario_n = seq(1:length(unique(df$cenario))))
df2 <- left_join(df, dic) 

# Lendo e numerando as seções
dic_sec <- read.csv2("csv/secoes.csv") 
dic_sec <- dic_sec %>% 
    mutate(local_n = seq(1:nrow(dic_sec))) 

# Loop para a geração dos gráficos
for(j in unique(df2$cenario_n)){
    df2$particula <- as.numeric(df2$particula)
    dfc <- df2 %>% filter(cenario_n==j)
    
    
    for (k in unique(dic_sec$local_n)){
        dic_sec2 <- dic_sec %>% filter(local_n==k)
        df_secao <- .FiltraSecao(dfc, dic_sec2[1,"X1"], dic_sec2[1,"Y1"], dic_sec2[1,"X3"], dic_sec2[1,"Y3"],dic_sec2[1,"d"])
        fundo <- .readFundo(dic_sec2[1,"dir"])
          
               
        
        # Seção em planta ---------------------------------------------------------
        
        
        plot1 <- 
            ggplot (data = NULL)+
            geom_point(data = dfc, 
                       aes(x = x, y = y, color = z), 
                       size =0.05, alpha = 0.5)+
            scale_color_gradientn(colors = c("#004d00","#ccffcc"))+
            geom_point(data = df_secao, 
                       aes(x = x, y = y), 
                       size =0.05, alpha = 0.5, color = '#ac39ac')+
            geom_line(aes(x = c(df_secao$x1[1],df_secao$x3[1]),
                          y = c(df_secao$y1[1],df_secao$y3[1])),
                      size =0.5,  color = '#ac39ac')+
            geom_path(data = contorno, 
                      aes(x = x,y = y), 
                      color = 'black', size =0.2)+
            labs (title = "(b) Reservatório em planta\n(seção transversal* em roxo)",
                  x = "Coordenada x",
                  y = "Coordenada y",
                  color = "Profundidade\nda partícula (m)")+
            coord_fixed()+
            scale_x_continuous(limits = c(314000,321000), expand = c(0,0))+
            scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))
        
        #plot1
        
        # Seção Transversal -------------------------------------------------------
        
        
        #.tmin <- min(df$t)
        #color = 24*(t-.tmin)
        
        plot2 <- df_secao %>% 
            mutate(d1 = sqrt(x1^2+y1^2),
                   d3 = sqrt(x3^2+y3^2),
                   dp = sqrt(x^2+y^2),
                   dm = dp-min(d1,d3),
                   dm_max = max(dm),
                   dm_min = min(dm)) %>% 
            ggplot ()+    
            geom_line(data = fundo,
                      aes(x = .OrientacaoX(dm, dm_max, dm_min, dic_sec2[1,"longitudinal"]), y = z), 
                      size =0.5, alpha = 0.5, color='black')+
            geom_line(aes(x = .OrientacaoX(dm, fundo$dm_max[1], fundo$dm_min[1], dic_sec2[1,"longitudinal"]), y = z, color = Destino, group =particula), 
                      size =0.3)+
            geom_point(aes(x = .OrientacaoX(dm, fundo$dm_max[1], fundo$dm_min[1], dic_sec2[1,"longitudinal"]), y = z, color = Destino, group=particula), 
                       size =0.8, alpha = 0.8, stroke = 1, shape=1)+
            scale_y_continuous(breaks = c(0,-20,-40), limits = c(-50,0), minor_breaks = c(seq(0,-50,by=-5)))+
            scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85","Reservatório"="#78BC2F"), drop = FALSE)+
            labs (title = "(c) Seção transversal*",
                  x = " ",
                  y = "Profundidade (m)",
                  color = "Destino")
        
        #plot2
        
        # Histograma ----------------------------------------------------------------------
        
        tabela <- df_secao %>% 
            mutate(z_group= #ifelse(z >= 0,  2.5,
                                   ifelse(z >= -5,  -2.5,
                                          ifelse(z >= -10,  -7.5,
                                                 ifelse(z >= -15, -12.5,
                                                        ifelse(z >= -20, -17.5,
                                                               ifelse(z >= -25, -22.5,
                                                                      ifelse(z >= -30, -27.5,
                                                                             ifelse(z >= -35, -32.5,
                                                                                    ifelse(z >= -40, -37.5,
                                                                                           ifelse(z >= -45, -42.5,
                                                                                                  ifelse(z >= -50, -47.5,
                                                                                                         ifelse(z >= -55, -52.5,
                                                                                                                ifelse(z >= -60, -57.5))))))))))))) %>%
            group_by(particula, z_group, cenario) %>%
            mutate(contagem = n())
        
        tabela <- tabela %>% 
            group_by(cenario) %>% 
            mutate(total_contagem = length(tabela$contagem)/length(unique(tabela$cenario)),
                   percentual = 1/ total_contagem*100) %>% 
            group_by(cenario, z_group) %>% 
            mutate(sum_percentual = sum(percentual),
                   y_pos = sum_percentual/100+0.05,
                   x_pos = z_group)
        
        verificacao<- tabela %>% 
            group_by(cenario, sum_percentual) %>% 
            summarise(sum_percentual= mean(sum_percentual)) %>% 
            summarise(sum_percentual= sum(sum_percentual))
        
        
        plot4 <-  tabela%>% 
            ggplot ()+
            geom_bar(aes(x= z_group, y = percentual/100), 
                     width = 4.8,
                     fill = "#ac39ac",
                     stat = "identity")+
            coord_flip()+
            labs(x = "Profundidade (m)", y = "Percentual de partículas (%)",title = "(a) Distribuição das partículas\nna seção transversal*")+
            scale_x_continuous(limits = c(-55,2))+
            scale_y_continuous(labels = scales::percent_format(accuracy =1.)) +
            geom_text(
                aes(
                    label = gsub("[.]",",",sprintf("%.1f%%",sum_percentual)),
                    x = x_pos,
                    y = y_pos
                ),
                color = 'grey30',
                size = 2.5,
                check_overlap = T
            )
        #plot4
        
        
        # Gráfico conjunto --------------------------------------------------------
        
        plot <- (plot4+plot1)/plot2+
            plot_annotation(
                title = paste0("Cenário ", dfc$cenario[1], " - Seção ", dic_sec2$Local[1]),
                #subtitle = "subtitle",
                caption = paste0("*Seção transversal entre as coordenadas UTM: (",
                                 round(df_secao$x1,0),", ",
                                 round(df_secao$y1,0),") e (",
                                 round(df_secao$x3,0),", ",
                                 round(df_secao$y3,0),")"),
                theme = theme(plot.caption = element_text(hjust = 0.2),
                              plot.title = element_text(hjust = 0.5,face="bold", size = 18),
                ))
        
        ggsave(
            filename = sprintf("%s [S%s].png",gsub("\n"," ",dfc$cenario[1]), dic_sec2$local_n[1])        path = nRTools::createDir(sprintf("img/[3x1] Seção Transversal/%s - todas", gsub('/', ' - ', nome))),
            plot = plot,
            device = "png",
            width = 20,
            height = 18,
            units = "cm"
        )
        
        
        
        k <- k+1
    }
    j <- j+1
}







