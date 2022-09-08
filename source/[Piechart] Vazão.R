# -------------------------------------------------------------------------
# Leitura do arquivo de vazões 
# -------------------------------------------------------------------------

vazao <- read.csv2("csv/vazoes_jul22.csv")


vazao_melt <- vazao %>% gather(key = "Local", value = "Vazao", c("CFME","CFMD", "VER", "VT"))



plot <- vazao_melt %>%  
  group_by(Cenário) %>% 
  mutate(Prop = Vazao / Total,
         soma_cumulativa = cumsum(Prop),
         posicao = soma_cumulativa/2,
         y_pos = 1 - (cumsum(Prop) - Prop / 2),
         x_pos = log(seq(2.5, 4, length.out = n()))
  ) %>% 
  mutate(Local= factor(Local, levels = c("CFME", "CFMD", "VER", "VT")))%>% 
  filter(Prop != 0) %>% 
  ggplot(aes(x = 1, y = Prop, fill = Local ))+
  geom_bar(stat = "identity",
           width = 1)+
  #scale_y_continuous(expand = c(0,0))+
  #scale_x_discrete(expand = c(0,0))+
  coord_polar(theta = "y")+
  facet_wrap(~Cenário, nrow=1)+
  #coord_flip()+
  geom_text(
    aes(
      label = gsub("[.]",",",sprintf("%.1f%%", Prop*100)),
      x = x_pos,
      y = y_pos
    ),
    size = 3,
    check_overlap = F
  )+
  labs(title = "Proporção de vazões em cada cenário")+
  scale_fill_manual(values= c("#49D4C6","#FEDB5D","#A096C5","#ED6E85","#71C194"))+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

plot
ggsave(
  filename = paste0("Proporção de vazões em cada cenário - ",gsub("^|[/][^/]*$", "", nome),".png"),
  path = nRTools::createDir("img/[Piechart] Distribuição das partículas"),
  plot = plot,
  device = "png",
  width = 22,
  height = 12,
  units = "cm"
)
