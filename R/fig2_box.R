
#### Fig 2 Box plots -----------------------


# 
# # *2.1 calculate distances from hex cells ----------
# 
# 
# # find index of closest BRT line
# nearest = st_nearest_feature(df_land, df_brt)
# 
# # calculate distance to closest BRT line
# df_land$dist_to_brt <- st_distance(df_land, df_brt[nearest,], by_element=TRUE)
# 
# # to Km
# df_land$dist_to_brt <- as.numeric(df_land$dist_to_brt/1000)
# summary(df_land$dist_to_brt)
# 
# # density
# df_land$areakm2 <- st_area(df_land) %>% as.numeric() / 1000000
# df_land$pop_density <- df_land$P001 / df_land$areakm2 
# 
# 
# #  *2.2 calculate distances from real state properties ----------
# 
# # find index of closest BRT line
# nearest_imb = st_nearest_feature(imobs, df_brt)
# # calculate distance to closest BRT line
# imobs$dist_to_brt <- st_distance(imobs, df_brt[nearest_imb,], by_element=TRUE)
# 
# # to Km
# imobs$dist_to_brt <- as.numeric(imobs$dist_to_brt/1000)
# summary(imobs$dist_to_brt)
# 
# 
# 
# #  *2.3 discretize distances ----------
# 
# # real state properties
# setDT(imobs)
# summary(imobs$dist_to_brt)
# imobs[, dist_to_brt_d := cut(x=dist_to_brt, 
#                              breaks=c(-1, .3, .6, 1, 2, 3, 5,10, 20),
#                              labels=c('0-0.3', '0.3-0.6', '0.6-1.0', '1-2', '1-3', '3-5', '5-10', '10+'))]
# 
# table(imobs$dist_to_brt_d, useNA = 'always')
# 
# # h3 grid
# setDT(df_land)
# summary(df_land$dist_to_brt)
# df_land[, dist_to_brt_d := cut(x=dist_to_brt, 
#                                breaks=c(-1, .3, .6, 1, 2, 3, 5,10, 20),
#                                labels=c('0-.3', '.3-.6', '.6-1', '1-2', '1-3', '3-5', '5-10', '10+'))]
# table(df_land$dist_to_brt_d, useNA = 'always')
# 
# subset(df_land, is.na(dist_to_brt_d))
# 
# 


#### ***fig.2 A - Pop density -----------------------
summary(subset(df_land, P001>0)$pop_density)







