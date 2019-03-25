library(tidyverse)
library(jpeg)
library(RColorBrewer)

radius <- 100 #pixels
pic <- readJPEG("./images/我的照片2.jpg") 
dim (pic)
p <- pic [600:1976, 1:1650, 1:3]
#####################################

#1 SCALE IMAGE ----
# Adapted from LEGO mosaics project
scale_image <- function(image, img_size){
  #Convert image to a data frame with RGB values
  img <- bind_rows(
    list(
      (as.data.frame(image[, , 1]) %>% 
         mutate(y=row_number(), channel = "R")),
      (as.data.frame(image[, , 2]) %>% 
         mutate(y=row_number(), channel = "G")),
      (as.data.frame(image[, , 3]) %>% 
         mutate(y=row_number(), channel = "B"))
    )
  ) %>% 
    gather(x, value, -y, -channel) %>% 
    mutate(x = as.numeric(gsub("V", "", x))) %>% 
    spread(channel, value)
  
  img_size <- round(img_size, 0)
  
  #Wide or tall image? Shortest side should be `img_size` pixels
  if(max(img$x) > max(img$y)){
    img_scale_x <-  max(img$x) / max(img$y)
    img_scale_y <- 1
  } else {
    img_scale_x <- 1
    img_scale_y <-  max(img$y) / max(img$x)
  }
  
  #If only 1 img_size value, create a square image
  if(length(img_size) == 1){
    img_size2 <- c(img_size, img_size)
  } else {
    img_size2 <- img_size[1:2]
    img_scale_x <- 1
    img_scale_y <- 1
  }
  
  #Rescale the image
  img2 <- img %>% 
    mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
           x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>% 
    select(-x, -y) %>% 
    group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    summarize_at(vars(R, G, B), list(~mean(.))) %>% 
    rowwise() %>% 
    mutate(color = rgb(R, G, B)) %>% 
    ungroup() %>% 
    #Center the image
    filter(x <= median(x) + img_size2[1]/2, x > median(x) - img_size2[1]/2,
           y <= median(y) + img_size2[2]/2, y > median(y) - img_size2[2]/2) %>%
    #Flip y
    mutate(y = (max(y) - y) + 1)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  
  return(out_list)
}

###########################
p <- p %>% 
  scale_image(radius * 2)

# p$Img_scaled %>% 
#   ggplot(aes(x=x, y=y, fill=color)) + 
#   geom_raster() +
#   scale_fill_identity(guide = FALSE) +
#   labs(title = "Scaled Me image", 
#        subtitle = "100px * 100px") +
#   geom_vline(xintercept = min(p$Img_scaled$x)+(max(p$Img_scaled$x)-min(p$Img_scaled$x))/2)+
#   geom_hline(yintercept = max(p$Img_scaled$y)/2)+
#   coord_fixed() +
#   theme_void()

# Function for equidistant points on a spiral

spiral_cartesian <- function(img_df, spiral_radius, num_coils, chord_length, rotation){
  img <- img_df$Img_scaled
  
  #Derive additional spiral specifications
  centerX <- median(img$x)
  centerY <- median(img$y)
  
  thetaMax <- num_coils * 2 * pi
  awayStep <- spiral_radius / thetaMax
  
  #While loop to keep drawing spiral until we hit thetaMax
  spiral <- tibble()
  theta <- chord_length/awayStep
  
  while(theta <= thetaMax){
    #How far away from center
    away = awayStep * theta
    
    #How far around the center
    around = theta + rotation
    
    #Convert 'around' and 'away' to X and Y.
    x = centerX + cos(around) * away
    y = centerY + sin(around) * away
    
    spiral <- spiral %>% 
      bind_rows(tibble(x=x, y=y))
    
    theta = theta + chord_length/away
  }
  
  return(c(img_df, list(spiral = spiral)))
}

p2 <- p %>% 
  spiral_cartesian(spiral_radius = radius, 
                   num_coils     = 40, #Spiral folds on itself 50 times
                   chord_length  = 4,  #Each point is 2 pixels apart
                   rotation      = 0   #No rotation
  )

p2$spiral %>% # preview of the spiral
  ggplot(aes(x=x, y=y)) +
  geom_path(lwd=1) +
  coord_fixed() + #Not polar!
  theme_void()

#Project the image onto the spiral
project_image <- function(img_df){
  dat <- img_df$spiral %>% 
    #Round each spiral point to nearest whole number
    mutate(xs = round(x), ys = round(y)) %>% 
    #Join on the rounded points
    left_join(img_df$Img_scaled %>% rename(xs=x, ys=y)) %>% 
    #Create greyscale - 0 is lightest, 1 is darkest
    mutate(grey = R+G+B,
           grey = (1- (grey / max(grey))))
  
  return(c(img_df, list(projected_spiral = dat)))
}

p2 <- p2 %>% 
  project_image()
rect <- data.frame (x=sample(30:max(p2$projected_spiral$x-30), 20), ymax=c(97, 100))
pal=brewer.pal(9, "PuBu")

ggplot(data=p2$projected_spiral, aes(x=x)) +
  geom_path(aes(x=x, y=y, color= grey), lwd=1) +
  geom_rect (aes (xmin=min(x)-40, xmax=max(x), ymin=min(y), ymax=max(y)+50), fill=NA, color=pal[9], size=30)+
  geom_rect(aes(xmin=min(x)-40, xmax=max(x), ymin=min(y), ymax=70), fill=pal[9])+
  annotate ("text", x=70, y= 225, size=20, color=pal[4], label="EX LIBRIS", family="American Typewriter", alpha= 0.6)+
  scale_color_gradientn (colors=pal[9:1])+
  annotate ("text", x=168, y= 65, size=3, color=pal[1], label="dispensable  XI/I  I/I", family="American Typewriter", alpha=0.8)+
  geom_segment (data=rect, aes(x=x,xend=x, y=60, yend=70), size=1, color=pal[1])+
  coord_fixed(expand = FALSE) +
  theme_void()+
  theme(legend.position="none", panel.background=element_rect(fill=pal[1]))


ggsave("p.png", path="./images")
