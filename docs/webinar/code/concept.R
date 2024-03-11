# source('./Code/concept.R')
library(raster)
library(graphicsutils)

random <- function(cont = TRUE) {
  m <- secr::make.mask(nx = 100, ny = 100, spacing = 20)
  h <- secr::randomHabitat(m, p = 0.5, A = 0.3)
  r <- secr::raster(h)

  if (cont) {
    r <- r * rnorm(length(r), 1, .2)
    r <- r / maxValue(r)
    return(r)
  }

  if (!cont) {
    return(r)
  }
}

s <- stack(list(s1 = random(), s2 = random(), s3 = random()))
h <- stack(list(h1 = random(F), h2 = random(F), h3 = random(F)))

# Impacts individuels
u <- matrix(nrow = 3, ncol = 3, data = runif(9, 1,2))
l <- list()
for(i in 1:3) {
  for(j in 1:3) {
    l <- c(l, s[[j]] * h[[i]] * u[i,j])
  }
}

l <- stack(l)


cols <- c("#ffffff","#C7CBCE", "#96A3A3", "#687677", "#222D3D", "#25364A", "#C77F20", "#E69831", "#E3AF16", "#E4BE29", "#F2EA8B")
pal <- colorRampPalette(cols)
pal2 <- colorRampPalette(c('#ffffff','#306919'))
box3 <- function(side) box2(side = side, which = 'figure', lty = 2)


mat <- matrix(nrow = 7, ncol = 7)
mat[1, ] <- c(0,1,2,2,2,3,0)
mat[2, ] <- c(0,17,9,10,11,12,0)
mat[3, ] <- c(4,13,18,19,20,27,8)
mat[4, ] <- c(4,14,21,22,23,28,8)
mat[5, ] <- c(4,15,24,25,26,29,8)
mat[6, ] <- c(0,16,30,31,32,33,0)
mat[7, ] <- c(0,5,6,6,6,7,0)


concept <- function(uid, nm) {
  # uid
  # 1: aire etude
  # 2:
  output <- paste0('./assets/img/concept',nm,'.png')
  png(output, res = 200, width = 200, height = 200, units = "mm")

  layout(mat, heights = c(.25,1,1,1,1,1,.25), widths = c(.25,1,1,1,1,1,.25))
  # layout.show(max(mat))

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Titles
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  par(mar = c(.5,.5,.5,.5))

  # Aire etude
  plot0()
  text(0,0,"Aire\nd'étude", adj = c(.5,.5), font = 2)

  #
  plot0()
  if(2 %in% uid) text(0,0,"Facteurs de stress", adj = c(.5,.5), font = 2)
  box3('24')

  #
  plot0()
  if(3 %in% uid) text(0,0,"Exposition\ncumulée", adj = c(.5,.5), font = 2)

  plot0()
  if(4 %in% uid) text(0,0,"Composantes valorisées", adj = c(.5,.5), font = 2, srt = 90)
  box3('13')

  plot0()
  if(5 %in% uid) text(0,0,"Composantes\nvalorisées intégrées", adj = c(.5,.5), font = 2)

  plot0()
  if(6 %in% uid) text(0,0,"Impacts intégrés stresseurs", adj = c(.5,.5), font = 2)
  box3('24')

  plot0()
  if(7 %in% uid) text(0,0,"Impacts cumulés", adj = c(.5,.5), font = 2)

  plot0()
  if(8 %in% uid) text(0,0,"Impacts intégrés\ncomposantes valorisées", adj = c(.5,.5), font = 2, srt = 90)
  box3('13')

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Stressors
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  par(mar = c(.5,.5,.5,.5))
  if(9 %in% uid) {
    image(s[[1]], col = pal(100), axes = F, xlab = '', ylab = ''); box(); box3('12')
  } else {
    plot0(); box3('12')
  }

  if(10 %in% uid) {
    image(s[[2]], col = pal(100), axes = F, xlab = '', ylab = ''); box(); box3('1')
  } else {
    plot0(); box3('1')
  }

  if(11 %in% uid) {
    image(s[[3]], col = pal(100), axes = F, xlab = '', ylab = ''); box(); box3('14')
  } else {
    plot0(); box3('14')
  }

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Empreinte cumulée
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  par(mar = c(.5,.5,.5,.5))
  if(12 %in% uid) {
    image(sum(s, na.rm = T), col = pal(100), axes = F, xlab = '', ylab = ''); box(); box3('1')
  } else {
    plot0(); box3('1')
  }



  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Composantes valorisées
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  par(mar = c(.5,.5,.5,.5))
  if(13 %in% uid) {
    image(h[[1]], col = '#BACDB2', axes = F, xlab = '', ylab = ''); box(); box3('34')
  } else {
    plot0(); box3('34')
  }

  if(14 %in% uid) {
    image(h[[2]], col = '#BACDB2', axes = F, xlab = '', ylab = ''); box(); box3('4')
  } else {
    plot0(); box3('4')
  }

  if(15 %in% uid) {
    image(h[[3]], col = '#BACDB2', axes = F, xlab = '', ylab = ''); box(); box3('14')
  } else {
    plot0(); box3('14')
  }

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Composantes valorisées intégrées
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  par(mar = c(.5,.5,.5,.5))
  if(16 %in% uid) {
    image(sum(h, na.rm = T), col = pal2(4), axes = F, xlab = '', ylab = ''); box(); box3('4')
  } else {
    plot0(); box3('4')
  }

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Aire d'étude
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  plot0(); box(); box3('1')


  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Individual impacts
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  par(mar = c(.5,.5,.5,.5))
  if(17 %in% uid) {
    image(l[[1]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(18 %in% uid) {
    image(l[[2]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(19 %in% uid) {
    image(l[[3]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(20 %in% uid) {
    image(l[[4]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(21 %in% uid) {
    image(l[[5]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(22 %in% uid) {
    image(l[[6]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(23 %in% uid) {
    image(l[[7]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(24 %in% uid) {
    image(l[[8]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }

  if(25 %in% uid) {
    image(l[[9]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
  } else {
    plot0()
  }




  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Composantes valorisées cumulées
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if(26 %in% uid) {
    image(sum(l[[c(1,2,3)]], na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('2')
  } else {
    plot0(); box3('2')
  }

  if(27 %in% uid) {
    image(sum(l[[c(4,5,6)]], na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('2')
  } else {
    plot0(); box3('2')
  }

  if(28 %in% uid) {
    image(sum(l[[c(7,8,9)]], na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('2')
  } else {
    plot0(); box3('2')
  }

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Stresseurs cumulés
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if(29 %in% uid) {
    image(sum(l[[c(1,4,7)]], na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('3')
  } else {
    plot0(); box3('3')
  }

  if(30 %in% uid) {
    image(sum(l[[c(2,5,8)]], na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('3')
  } else {
    plot0(); box3('3')
  }

  if(31 %in% uid) {
    image(sum(l[[c(3,6,9)]], na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('3')
  } else {
    plot0(); box3('3')
  }


  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  # Impacts cumulés
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  if(32 %in% uid) {
    image(sum(l, na.rm = T), axes = F, col = pal(100), xlab = '', ylab = ''); box(); box3('23')
  } else {
    plot0(); box3('23')
  }



  dev.off()

}

concept(uid = c(1,2,4,9,13,17), nm = 2)
concept(uid = c(1,2,4,9,13,10,11,17,18,19), nm = 3)
concept(uid = c(1,2,4,9,13,10,11,17,18,19,26,8), nm = 4)
concept(uid = c(1,2,4,9,13,14,15,17,20,23,29,6), nm = 5)
concept(uid = c(1,2,4,6,8,9:11,13:15,17:31), nm = 6)
concept(uid = c(1,2,4,6:8,9:11,13:15,17:32), nm = 7)
concept(uid = c(1:3,9:12), nm = 8)
concept(uid = c(1,4,5,13:16), nm = 9)
concept(uid = 1:32, nm = 10)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Concept 1
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
output <- paste0('./assets/img/concept',1,'.png')

mat <- matrix(0, ncol = 7, nrow = 2)
mat[1,1] <- 1
mat[1,3] <- 2
mat[1,5] <- 3
mat[1,7] <- 4
mat[2,2] <- 5
mat[2,4] <- 6
mat[2,5] <- 7
mat[2,6] <- 8
mat[2,1] <- 9
mat[2,3] <- 10
mat[2,7] <- 11

png(output, res = 200, width = 200, height = 50, units = "mm")
layout(mat, widths = c(1,.2,1,.2,1,.2,1), heights = c(.25,1))

# Text
# Aire etude
par(mar = c(0,0,0,0))
plot0()
text(0,0,"Facteur\nde stress", adj = c(.5,.5), font = 2, cex = 1.5)

plot0()
text(0,0,"Composante\nvalorisée", adj = c(.5,.5), font = 2, cex = 1.5)

plot0()
text(0,0,"Vulnérabilité", adj = c(.5,.5), font = 2, cex = 1.5)

plot0()
text(0,0,"Effet", adj = c(.5,.5), font = 2, cex = 1.5)

# "Math"
library(latex2exp)
par(mar = c(0,0,0,0))
for(i in 1:2) {
  plot0()
  text(0,0,TeX("$*$"), adj = c(.5,.5), font = 2, cex = 5)
}

plot0()
text(0,0,TeX("$\\mu"), adj = c(.5,.5), font = 2, cex = 7)

plot0()
text(0,0,TeX("$=$"), adj = c(.5,.5), font = 2, cex = 5)

# Rasters
par(mar = c(1,1,1,1))
image(s[[1]], col = pal(100), axes = F, xlab = '', ylab = ''); box()
image(h[[1]], col = '#BACDB2', axes = F, xlab = '', ylab = ''); box()
image(l[[1]], col = pal(100), axes = F, xlab = '', ylab = ''); box()


dev.off()



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Zone d'étude
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
output <- paste0('./assets/img/zone_etude.png')

mat <- matrix(0, ncol = 7, nrow = 2)
mat[1,1] <- 1
mat[2,1] <- 2

png(output, res = 200, width = 200, height = 50, units = "mm")
layout(mat, widths = c(1,.2,1,.2,1,.2,1), heights = c(.25,1))

# Aire etude
par(mar = c(0,0,0,0))
plot0()
text(0,0,"Zone\nd'étude", adj = c(.5,.5), font = 2, cex = 1.5)

par(mar = c(1,1,1,1))
plot0()
box()
dev.off()
