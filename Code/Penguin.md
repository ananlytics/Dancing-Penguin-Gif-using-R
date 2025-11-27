# ───── ROTATING GIF ─────
library(palmerpenguins)
if (!require(magick)) install.packages("magick")
library(magick)

df <- na.omit(penguins)[order(penguins$body_mass_g), ]
species_colors <- c("Adelie" = "#FF6F00", "Chinstrap" = "#8E44AD", "Gentoo" = "#2980B9")

df$island_factor <- as.numeric(df$island)
df$theta <- df$island_factor * 2*pi/3 + (1:nrow(df)) * 0.08
df$radius <- df$body_mass_g / 1000
df$point_size <- 1 + 3 * (df$flipper_length_mm - min(df$flipper_length_mm)) /
  (max(df$flipper_length_mm) - min(df$flipper_length_mm))

add_alpha <- function(col, alpha=0.5) rgb(col2rgb(col)/255, alpha=alpha)

desktop <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
gif_final <- file.path(desktop, "PENGUIN_GALAXY_SPINNING_FINAL.gif")

cat("Creating 60 rotating frames...\n")

frames <- lapply(1:60, function(i) {
  angle <- 2 * pi * (i-1) / 60          # this line is the key fix
  
  png(tmp <- tempfile(fileext = ".png"), width=1000, height=1000, bg="#0A0A1F")
  par(mar=rep(0,4), bg="#0A0A1F")
  plot(1, type="n", xlim=c(-5.2,5.2), ylim=c(-5.2,5.2), axes=F, xlab="", ylab="", asp=1)
  
  x <- df$radius * cos(df$theta + angle)
  y <- df$radius * sin(df$theta + angle)
  
  points(x, y, pch=19, cex=df$point_size*5,   col=add_alpha(species_colors[df$species], 0.25))
  points(x, y, pch=19, cex=df$point_size*1.8, col=species_colors[df$species])
  text(0, 4.3, "Palmer Penguins\nSpiral Galaxy", col="white", cex=3, font=2)
  dev.off()
  
  image_read(tmp)
})

cat("Combining into smooth spinning GIF...\n")
gif <- image_animate(image_join(frames), fps = 20)
image_write(gif, gif_final)

shell.exec(desktop)

```
![PENGUIN_GALAXY_SPINNING_FINAL](https://github.com/user-attachments/assets/8b8a20e6-9c40-48f1-a761-e267492d9598)

<img width="1000" height="1000" alt="PENGUIN_GALAXY_ROTATING_NOW" src="https://github.com/user-attachments/assets/ffea4ece-09e4-4a42-997d-76f6c8a26099" />



