

    tabela%>% 
    group_by(particula,cenario) %>%
    mutate(total_contagem = n())



  aux <- tabela%>% 
    group_by(particula,cenario) %>%
    count()


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

aux1 <- tabela%>% 
  group_by(particula,cenario) %>%
  count(name = 'n_cenario_particula')

aux2 <- tabela%>% 
  group_by(particula) %>%
  count(name = 'n_particula')

aux3 <- tabela%>% 
  group_by(cenario) %>%
  count(name = 'n_cenario')

tabela2 <- left_join(tabela, aux1)

tabela3 <- left_join(tabela2, aux2)

tabela4 <- left_join(tabela3, aux3)

tabela5 <- tabela4 %>% 
    mutate(
      percentual_acum = n_particula/ n_cenario,
      percentual = n_cenario_particula/ n_particula) %>% 
    group_by(cenario, z_group) %>% 
    mutate(sum_percentual_acum = sum(percentual_acum),
           y_pos = sum_percentual_acum/100+0.05,
           x_pos = z_group)
verificacao<- tabela5 %>% 
    group_by(cenario, sum_percentual_acum) %>% 
    summarise(sum_percentual_acum= mean(sum_percentual_acum)) %>% 
    summarise(sum_percentual_acum= sum(sum_percentual_acum))
verificacao

plot3 <-  tabela5%>% 
    ggplot ()+
    geom_bar(aes(x= z_group, y= percentual_acum/100), 
             width = 4.8,
             fill = "#ac39ac",
             stat = "identity")+
    coord_flip()+
    labs(x = "Profundidade (m)", y = "Percentual de partículas (%)")+
    scale_x_continuous(limits = c(-55,2))+
    scale_y_continuous(limits = c(0,0.55),labels = scales::percent_format(accuracy =1.)) +
    facet_wrap(.~cenario, nrow=2)+
    geom_text(
        aes(
            label = gsub("[.]",",",sprintf("%.1f%%", sum_percentual_acum)),
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
    width = 25,
    height = 15,
    units = "cm"
)


