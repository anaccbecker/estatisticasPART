
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



contagem <- function (tabela, j){
n <- tabela %>%  
    filter(cenario_n==j) %>% 
    nrow() %>% 
    as.numeric()
return(n)
}

tabela2<- data.frame(cenario_n = unique(tabela$cenario_n),
           contagem_cenario = NA) 
for ( i in unique(tabela$cenario_n)){
    tabela2[i,"contagem_cenario"] <-
        tabela %>% contagem(i)
}

tabela <- left_join(tabela, tabela2)
tabela <- tabela %>% 
    group_by(cenario) %>% 
    mutate(percentual = 1/ contagem_cenario*100) %>% 
    group_by(cenario, z_group) %>% 
    mutate(sum_percentual = sum(percentual),
           y_pos = sum_percentual/100+0.09,
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
    scale_x_continuous(limits = c(-50,2))+
    scale_y_continuous(limits = c(0,1),labels = scales::percent_format(accuracy =1.)) +
    facet_wrap(.~cenario, nrow=4)+
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

plot3

ggsave(
    filename = paste0(gsub('/', ' - ', nome),".png"),
    path = nRTools::createDir("img/[Histogram] Profundidades"),
    plot = plot3,
    device = "png",
    width = 20,
    height = 28,
    units = "cm"
)


