

tab$particula<- as.numeric(tab$particula)

plot <- tab  %>% 
    mutate(Tempo.Lancamento = Tempo.Chegada - Tempo.Trajeto) %>% 
    gather(key = "Tipo_Tempo", value = "Tempo", 4:6) %>% 
    filter(Tipo_Tempo == "Tempo.Chegada"|Tipo_Tempo == "Tempo.Lancamento") %>% 
    #mutate( Tipo_Tempo = replace(Tipo_Tempo, "Tempo.Chegada", "Tempo de chegada"))%>%  
    #mutate( ~replace(., "Tempo.Lancamento", "Tempo de lançamento"))%>% 
    #filter(particula<=72) %>% 
    ggplot()+
    geom_point(aes(as.factor(particula), Tempo, color=Destino, shape = Tipo_Tempo))+
    facet_wrap(.~cenario, nrow=8)+
    scale_color_manual(values= c("#49D4C6","#FEDB5D","#A096C5","#ED6E85","#71C194"))+
    labs(x= "Número da partícula",
         y = "Tempo (h)",
         shape= "")
plot  

ggsave(
    filename = "Cenários Operacionais - sem log boom - lançamento superície.png",
    path = nRTools::createDir("img/[Dotchart] Tempo das partículas"),
    plot = plot,
    device = "png",
    width = 40,
    height = 25,
    units = "cm"
)
