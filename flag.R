library("tidyverse")

x = seq(0, 1, 1e-4)
orange = 45 + 2*cos(x = 20*x %% 1)/2

angles = seq(-2*pi, 2*pi, 4*pi/length(x))[1:length(x)]

df = data.frame(x = x, 
                orange = orange,
                white = orange - 6,
                green = orange - 12) %>% 
  mutate(pole = seq(0, 49, 49/length(x))[1:length(x)]) %>% 
  mutate(chakra_x = 0.16 + 0.04*cos(angles),
         chakra_y = 38 + 3*sin(angles))

spoke_angles = seq(0, 2*pi, 2*pi/24)

get_spoke_points = function(angle) {
  start_x = 0.16
  end_x = 0.16 + 0.04*cos(angle)
  by_x = (end_x - start_x)/1e4
  x_seq = 0.16
  
  if (cos(angle) != 0) {
    x_seq = seq(start_x, end_x, by_x)
  }
  
  start_y = 38
  end_y = 38 + 3*sin(angle)
  by_y = (end_y - start_y)/1e4
  
  y_seq = 38
  
  if (sin(angle) != 0) {
    y_seq = seq(start_y, end_y, by_y)
  }

  spoke_points = data.frame(x = x_seq, y = y_seq)
    
  return(spoke_points)  
}

spokes = parallel::mclapply(spoke_angles, get_spoke_points) %>% bind_rows()
spoke_sample = sample_n(tbl = spokes, size = nrow(df))

flag = df %>% 
  ggplot() +
  geom_line(aes(x = x, y = orange, group = 1), size = 30, color = "#FF9933") +
  geom_line(aes(x = x, y = white, group = 1), size = 30, color = "#FFFFFF") +
  geom_line(aes(x = x, y = green, group = 1), size = 30, color = "#138808") +
  geom_point(aes(x = 0, y = pole), size = 5, color = "#826a4b") +
  geom_point(aes(x = chakra_x, y = chakra_y), color = "#000080", size = 0.8) +
  geom_point(aes(x = spoke_sample$x, y = spoke_sample$y), size = 0.3, color = "#000080") +
  ylim(0, 50) +
  xlim(0, 0.31) +
  xlab("") +
  ylab("") +
  theme(text = element_blank())

flag

aspect_ratio <- 31.04/58.91
h <- 8.5
ggsave(flag, height = h , width = h * aspect_ratio, filename = "flag.png")
