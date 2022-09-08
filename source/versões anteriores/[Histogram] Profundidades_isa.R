
tabela <- df %>% 
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

plot3 <-  tabela%>% 
    ggplot ()+
    geom_bar(aes(x= z_group, y = percentual/100), 
             width = 4.8,
             fill = "#ac39ac",
             stat = "identity")+
    coord_flip()+
    labs(x = "Profundidade (m)", y = "Percentual de partículas (%)")+
    scale_x_continuous(limits = c(-55,2))+
    scale_y_continuous(limits = c(0,0.55),labels = scales::percent_format(accuracy =1.)) +
    facet_wrap(.~cenario, nrow=1)+
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

#plot3

ggsave(
    filename = paste0(gsub('/', ' - ', nome),".png"),
    path = nRTools::createDir("img/[Histogram] Profundidades"),
    plot = plot3,
    device = "png",
    width = 25,
    height = 15,
    units = "cm"
)


