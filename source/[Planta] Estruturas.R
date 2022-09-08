

plot1<- ggplot()+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (
        x = "Coordenada x",
        y = "Coordenada y",
        title = "Sem estruturas de retenção")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    theme(plot.title = element_text(size = 12))

plot2<- ggplot()+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (
        x = "Coordenada x",
        y = "Coordenada y",
        title = "Com log boom")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    geom_path(data = lbl1, 
              aes(x = x,y = y), 
              color = 'red', size =0.8)+
    geom_path(data = lbl2, 
              aes(x = x,y = y), 
              color = 'red', size =0.8)+
    theme(plot.title = element_text(size = 12))

plot3<- ggplot()+
    geom_path(data = contorno, 
              aes(x = x,y = y), 
              color = 'black', size =0.2)+
    labs (
        x = "Coordenada x",
        y = "Coordenada y",
        title = "Com log boom e retentor/desviador")+
    coord_fixed()+
    scale_x_continuous(limits = c(314000,321000), breaks = c(316000,318000,320000), expand = c(0,0))+
    scale_y_continuous(limits = c(8971000,8979000), expand = c(0,0))+
    geom_path(data = lbl1, 
              aes(x = x,y = y), 
              color = 'red', size =0.8)+
    geom_path(data = lbl2, 
              aes(x = x,y = y), 
              color = 'red', size =0.8)+
    geom_path(data = rdmd, 
              aes(x = x,y = y), 
              color = 'blue', size =0.8)+
    geom_path(data = rdme, 
              aes(x = x,y = y), 
              color = 'blue', size =0.8)+
    theme(plot.title = element_text(size = 12))


plot <- plot1+plot2+plot3

ggsave(
    filename = "Estruturas.png",
    path = "img",
    plot = plot,
    device = "png",
    width = 30,
    height = 10,
    units = "cm"
)

