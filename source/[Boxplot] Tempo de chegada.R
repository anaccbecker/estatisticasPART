
number <- tab%>%  
  group_by(cenario, Destino) %>%
  count()
tab2<- left_join(tab,number)

  plot <- tab2  %>% 
          ggplot(aes(Destino, Tempo.Trajeto, color = Destino, fill = Destino))+
          stat_boxplot(geom='errorbar', linetype=1, width=0.5)+
          geom_boxplot()+
          #geom_jitter(alpha=0.6)+
          facet_wrap(.~cenario, nrow=4)+
          scale_y_continuous(limits = c(0,max(tab$Tempo.Trajeto)+0.5), expand = c(0,0), breaks = c(0,2,4,6,8,10,12))+
          scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#e3be38","VER"="#A096C5","VT"="#ED6E85"), drop = FALSE)+
          scale_fill_manual(values= c("CFME"="#c3f1ec","CFMD"="#FEedac","VER"="#cec9e1","VT"="#f4a2b1"), drop = FALSE)+
          labs(y="Tempo de trajeto (h)")+
          theme(legend.position = "none")+
          geom_text(
            aes(
              label = sprintf("n = %s", n),
              x = Destino,
              y = 0.5
            ),
            size = 3,
            check_overlap = T,
            #color = "black", 
            fontface = "bold"
          )
  plot    
  ggsave(
      filename =  paste0(gsub('/', ' - ', nome),".png"),
      path = nRTools::createDir("img/[Boxplot] Tempo de chegada"),
      plot = plot,
      device = "png",
      width = 20,
      height = 28,
      units = "cm"
  )


## Gera tabela com as medianas
# tab%>%  
# group_by(cenario, Destino) %>%
# summarise(median = median(Tempo.Trajeto)) %>% 
# spread(key = Destino, value = median) %>% 
# write.csv2("output/medianas.csv")


#mean(tab$Tempo.Trajeto)
#median(tab$Tempo.Trajeto)


#2,5 km  2,2 h
# x      1 h

#2.5/mean(tab$Tempo.Trajeto)
# 1 m/s velocidade média


#plot <- tab  %>% 
#  ggplot(aes(Destino, Tempo.Trajeto, color = Destino) )+
#  geom_jitter(alpha=0.6)+
#  facet_wrap(.~cenario, nrow=2)+
#  scale_color_manual(values= c("CFME"="#49D4C6","CFMD"="#FEDB5D","VER"="#A096C5","VT"="#ED6E85"), drop = FALSE)+
#  labs(y="Tempo de trajeto (h)")+
#  theme(legend.position = "none")
#plot    
#ggsave(
#  filename = paste0(gsub('/', ' - ', nome),".png"),
#  path = nRTools::createDir("img/[Jitter] Tempo de chegada"),
#  plot = plot,
#  device = "png",
#  width = 20,
#  height = 15,
#  units = "cm"
#)
