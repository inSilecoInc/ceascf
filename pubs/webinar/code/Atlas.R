# source('./Code/Atlas.R')
library(graphicsutils)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Figure layout
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

mat <- matrix(0, ncol = 17, nrow = 19)

# Titles
mat[1, 1:8] <- 1
mat[1, 9:17] <- 2

# Subtitles
mat[2, 1:8] <- 3
mat[5, 1:8] <- 4
mat[8, 1:8] <- 5
mat[11, 1:8] <- 6
mat[14, 1:8] <- 7
mat[17, 1:8] <- 8
mat[2, 9:17] <- 9
mat[5, 9:17] <- 10
mat[8, 9:17] <- 11
mat[11, 9:17] <- 12
mat[14, 9:17] <- 13

# Rasters
# Stressors
mat[3:4,2:3] <- 14
mat[6:7,2:3] <- 15
mat[9:10,2:3] <- 16
mat[9:10,4:5] <- 17
mat[12:13,2:3] <- 18
mat[15:16,2:3] <- 19
mat[15:16,4:5] <- 20
mat[15:16,6:7] <- 21
mat[18:19,2:3] <- 22
mat[18:19,4:5] <- 23

# Valued components
mat[3:4,10:11] <- 24
mat[3:4,12:13] <- 25
mat[6:7,10:11] <- 26
mat[6:7,12:13] <- 27
mat[6:7,14:15] <- 28
mat[6:7,16:17] <- 29
mat[9:10,10:11] <- 30
mat[9:10,12:13] <- 31
mat[9:10,14:15] <- 32
mat[12:13,10:11] <- 33
mat[12:13,12:13] <- 34
mat[15:16,10:11] <- 35
mat[15:16,12:13] <- 36
mat[15:16,14:15] <- 37
mat[15:16,16:17] <- 38

# Zone étude
mat2 <- matrix(0, ncol = 4, nrow = 19)
mat2[1,1:4] <- 39
mat2[3:4, 2:3] <- 40

mat <- cbind(mat2,mat)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Text
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Titles
titles <- c('Stresseurs','Composantes valorisées')

# Subtitles
sub <- c('Dragage','Ancrages','Échouements et naufrages','Déversements accidentels',
         'Navigation','Engins de pêche',
         "Qualité de l'eau",'Habitats fauniques et floristiques',"Sites d'importance",
         'Intégrité des berges','Mammifères marins')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Colors
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cols <- c("#ffffff","#C7CBCE", "#96A3A3", "#687677", "#222D3D", "#25364A", "#C77F20", "#E69831", "#E3AF16", "#E4BE29", "#F2EA8B")
pal <- colorRampPalette(cols)

palRandom <- function() {
  hue <- 75
  col1 <- rainbow(255)[round(runif(1,0,255))]
  col2 <- rainbow(255)[round(runif(1,0,255))]
  cols <- c(darken(col1,hue), lighten(col1, hue),
            lighten(col2, hue), darken(col2, hue))
  colorRampPalette(cols)
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Rasters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# # Rasters
# library(raster)
#
# random <- function(cont = TRUE) {
#   m <- secr::make.mask(nx = 100, ny = 100, spacing = 20)
#   h <- secr::randomHabitat(m, p = 0.55, A = 0.4)
#   r <- secr::raster(h)
#
#   if (cont) {
#     # r <- r * rnorm(length(r), 1, .2)
#     r <- r * runif(length(r), 0, 1)
#     r <- r / maxValue(r)
#     return(r)
#   }
#
#   if (!cont) {
#     return(r)
#   }
# }
#
# s <- cv <-  list()
# for(i in 1:10) s[[i]] <- random()
# for(i in 1:15) cv[[i]] <- random(F)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Plot 1
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
png('./assets/img/atlas.png', res = 200, width = 250, height = 200, units = "mm")
w <- .9
w2 <- .5
h <- .5
layout(mat, widths = c(w,w,1,1,w,w,1,1,1,1,1,1,w,w,w,1,1,1,1,1,1),
            heights = c(h,h,1,1,h,1,1,h,1,1,h,1,1,h,1,1,h,1,1))

# Plot titles
for(i in titles) {
  par(mar = c(0,0,0,0))
  plot0()
  text(x = -.95, y = -.4, adj = c(0,.5), labels = i, font = 2, cex = 2)
}

# Plot subtitles
for(i in sub) {
  par(mar = c(0,0,0,0))
  plot0()
  text(x = -.85, y = -.2, adj = c(0,.5), labels = i, font = 2, cex = 1.5)
}


# Rasters
# Stressors
for(i in 1:10) {
  par(mar = c(.5,.5,.5,.5))
  image(s[[i]], col = palRandom()(100), axes = F, xlab = '', ylab = ''); box()
}

# Valued components
for(i in 1:15) {
  par(mar = c(.5,.5,.5,.5))
  image(cv[[i]], col = palRandom()(100), axes = F, xlab = '', ylab = ''); box()
}

# Zone étude
par(mar = c(0,0,0,0))
plot0()
text(x = -.95, y = -.4, adj = c(0,.5), labels = "Zone d'étude", font = 2, cex = 2)

# Box
par(mar = c(.5,.5,.5,.5))
plot0(); box()

dev.off()


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Plot 2
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
png('./assets/img/atlas2.png', res = 200, width = 250, height = 200, units = "mm")
w <- .9
w2 <- .5
h <- .5
layout(mat, widths = c(w,w,1,1,w,w,1,1,1,1,1,1,w,w,w,1,1,1,1,1,1),
            heights = c(h,h,1,1,h,1,1,h,1,1,h,1,1,h,1,1,h,1,1))

# Plot titles
for(i in titles) {
  par(mar = c(0,0,0,0))
  plot0()
  text(x = -.95, y = -.4, adj = c(0,.5), labels = i, font = 2, cex = 2)
}

# Plot subtitles
for(i in sub) {
  par(mar = c(0,0,0,0))
  plot0()
  text(x = -.85, y = -.2, adj = c(0,.5), labels = i, font = 2, cex = 1.5)
}


# Rasters
# Stressors
for(i in 1:10) {
  par(mar = c(.5,.5,.5,.5))
  image(s[[i]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
}

# Valued components
for(i in 1:15) {
  par(mar = c(.5,.5,.5,.5))
  image(cv[[i]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
}

# Zone étude
par(mar = c(0,0,0,0))
plot0()
text(x = -.95, y = -.4, adj = c(0,.5), labels = "Zone d'étude", font = 2, cex = 2)

# Box
par(mar = c(.5,.5,.5,.5))
plot0(); box()

dev.off()

# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# # Plot 3
# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# mat <- matrix(0, nrow = 17, ncol = 12)
# mat[3:nrow(mat), 1] <- 1
# mat[1, 3:ncol(mat)] <- 2
# mat[3:nrow(mat), 2] <- 3:17
# mat[2, 3:ncol(mat)] <- 18:27
#
#
# png('./assets/img/atlas3.png', res = 200, width = 210, height = 300, units = "mm")
# layout(mat, widths = c(.5, rep(1, 16)), heights = c(.5,rep(1,11)))
#
# # Titles
# par(mar = c(0,0,0,0))
# plot0()
# text(x = 0, y = 0, adj = c(.5,.5), labels = 'Composantes valorisées', font = 2, cex = 2, srt = 90)
#
# # Titles
# par(mar = c(0,0,0,0))
# plot0()
# text(x = 0, y = 0, adj = c(.5,.5), labels = 'Stresseurs', font = 2, cex = 2)
#
# # Rasters
# # Valued components
# for(i in 1:15) {
#   par(mar = c(.5,.5,.5,.5))
#   image(cv[[i]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
# }
#
# # Stressors
# for(i in 1:10) {
#   par(mar = c(.5,.5,.5,.5))
#   image(s[[i]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
# }
#
# dev.off()
