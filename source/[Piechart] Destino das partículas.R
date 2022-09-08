
if ("VT" %in% unique(df$Destino)){
    dflm <-tab%>%  
        group_by(cenario, Destino) %>%
        count()%>% 
        spread(key= "Destino", value = "n") %>% 
        mutate_all( ~replace(., is.na(.), 0)) %>% 
        mutate(total = max(unique(as.numeric(df$particula))),
               Reservatório = total-CFMD-CFME-VER-VT) %>% 
        gather(key = "Local", value = "Particulas", c("CFME","CFMD", "VER","VT","Reservatório"))
    
    plot <- dflm  %>%     
        group_by(cenario) %>% 
        mutate(Prop = Particulas / total,
               soma_cumulativa = cumsum(Prop),
               posicao = soma_cumulativa/2,
               y_pos = 1 - (cumsum(Prop) - Prop / 2),
               x_pos = log(seq(2.5, 4, length.out = n()))
        ) %>% 
        mutate(Local= factor(Local, levels = c("CFME", "CFMD", "VER", "VT", "Reservatório")))%>% 
        filter(Prop != 0) %>% 
        ggplot(aes(x = 1, y = Prop, fill = Local ))+
        geom_bar(stat = "identity",
                 width = 1)+
        coord_polar(theta = "y")+
        facet_wrap(.~cenario, nrow=4)+
        geom_text(
            aes(
                label = gsub("[.]",",",sprintf("%.f%%", Prop*100)),
                x = x_pos,
                y = y_pos
            ),
            size = 3,
            check_overlap = T
        )+
        labs(title = "Proporção do destino das partículas em cada cenário")+
        scale_fill_manual(values= c("#49D4C6","#FEDB5D","#A096C5","#ED6E85","#71C194"))+
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
        )
}else {
    dflm <-tab%>%  
        group_by(cenario, Destino) %>%
        count()%>% 
        spread(key= "Destino", value = "n") %>% 
        mutate_all( ~replace(., is.na(.), 0)) %>% 
        mutate(total = max(unique(as.numeric(df$particula))),
               Reservatório = total-CFMD-CFME-VER) %>% 
        gather(key = "Local", value = "Particulas", c("CFME","CFMD", "VER","Reservatório"))
    
    plot <- dflm  %>%     
        group_by(cenario) %>% 
        mutate(Prop = Particulas / total,
               soma_cumulativa = cumsum(Prop),
               posicao = soma_cumulativa/2,
               y_pos = 1 - (cumsum(Prop) - Prop / 2),
               x_pos = log(seq(2.5, 4, length.out = n()))
        ) %>% 
        mutate(Local= factor(Local, levels = c("CFME", "CFMD", "VER", "Reservatório")))%>% 
        filter(Prop != 0) %>% 
        ggplot(aes(x = 1, y = Prop, fill = Local ))+
        geom_bar(stat = "identity",
                 width = 1)+
        coord_polar(theta = "y")+
        facet_wrap(.~cenario, nrow=4)+
        geom_text(
            aes(
                label = gsub("[.]",",",sprintf("%.f%%", Prop*100)),
                x = x_pos,
                y = y_pos
            ),
            size = 3,
            check_overlap = T
        )+
        labs(title = "Proporção do destino das partículas em cada cenário")+
        scale_fill_manual(values= c("#49D4C6","#FEDB5D","#A096C5","#71C194"))+
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
        )
}

plot
ggsave(
    filename = paste0(gsub('/', ' - ', nome),".png"),
    path = nRTools::createDir("img/[Piechart] Distribuição das partículas"),
    plot = plot,
    device = "png",
    width = 20,
    height = 28,
    units = "cm"
)
