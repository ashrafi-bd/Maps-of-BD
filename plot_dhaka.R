
library(here)

# 0. Install (run once) & load

library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggspatial)
library(grid)
library(viridis)

# 1. Define data paths
shp_div1   <- here("data","bgd_admbnda_adm1_bbs_20201113.shp")
shp_dhaka  <- here("data","Dhaka_13_Districts.shp")

# 2. Read division shapefile
bd_div1 <- st_read(shp_div1)

# 3. Filter to 8 divisions
divs_8 <- c("Barisal","Chittagong","Dhaka","Khulna",
            "Mymensingh","Rajshahi","Rangpur","Sylhet")
bd8    <- bd_div1 %>% filter(ADM1_EN %in% divs_8)


# 4. Read Dhaka’s 13-district shapefile
dhaka13 <- st_read(here("data","Dhaka_13_Districts.shp"))
dhaka13 <- st_read(shp_dhaka)

# 5. Compute padding around Dhaka
bb     <- st_bbox(dhaka13)
pad_x  <- (bb$xmax - bb$xmin) * 0.1
pad_y  <- (bb$ymax - bb$ymin) * 0.1

# 6. A theme with axes & no grid
latlong_theme <- theme_bw() +
  theme(text            = element_text(family="Times New Roman"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title      = element_text(hjust=0.5, size=14)) +
  labs(x="Longitude", y="Latitude")

# 7. Highlight Dhaka on full‐country map
bd8 <- bd8 %>% mutate(highlight = if_else(ADM1_EN=="Dhaka","Dhaka","Other"))

# 8. Plot full Bangladesh
map_full <- ggplot(bd8) +
  geom_sf(aes(fill=highlight), color="black", size=0.25) +
  scale_fill_manual(values=c(Other="yellow",Dhaka="green"), guide=FALSE) +
  annotation_scale(location="bl", width_hint=0.3) +
  annotation_north_arrow(location="bl", which_north="true",
                         style=north_arrow_fancy_orienteering(),
                         pad_x=unit(0.5,"cm"), pad_y=unit(1.0,"cm")) +
  latlong_theme +
  ggtitle("Bangladesh")

print(map_full)

ggsave(
  filename = here("output","dhaka_latlong.png"),
  plot     = combined,
  width    = 12, height = 6, dpi = 300, bg = "white"
)