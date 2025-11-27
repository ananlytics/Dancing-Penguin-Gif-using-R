# ───── FINAL WORKING ROTATING GIF ─────
library(palmerpenguins)                                 # Loading the dataset that contains 344 real penguins
if (!require(magick)) install.packages("magick")        # If magick package not installed → installing it
library(magick)                                         # Load magick – to read PNGs and turn them into GIFs

df <- na.omit(penguins)[order(penguins$body_mass_g), ]  # Remove rows with missing data, then sort penguins from lightest to heaviest
species_colors <- c("Adelie" = "#FF6F00",               # Assign a unique bright color to each penguin species
                    "Chinstrap" = "#8E44AD", 
                    "Gentoo" = "#2980B9")

df$island_factor <- as.numeric(df$island)               # Converting island name (Biscoe, Dream, Torgersen) → numbers 1,2,3
df$theta <- df$island_factor * 2*pi/3 + (1:nrow(df)) * 0.08  
# This is the heart of the spiral:
#   • 2π/3 = 120° → each island gets its own spiral arm, separated by 120°
#   • + (1:nrow(df))*0.08 → slowly increases angle as we go down the data → creates the spiral shape

df$radius <- df$body_mass_g / 1000                      # Heavier penguins to go farther from center
df$point_size <- 1 + 3 * (df$flipper_length_mm - min(df$flipper_length_mm)) /
                        (max(df$flipper_length_mm) - min(df$flipper_length_mm))
# Makes penguins with longer flippers have bigger dots (scaled from ~1 to ~4)

add_alpha <- function(col, alpha=0.5) rgb(col2rgb(col)/255, alpha=alpha)
# Tiny helper function to draw semi-transparent “glow” dots in base R

desktop <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
# exact path to the destination folder

gif_final <- file.path(desktop, "PENGUIN_GALAXY_SPINNING_FINAL.gif")
# Full path + filename of the final GIF

cat("Creating 60 rotating frames...\n")                 # Friendly progress message

frames <- lapply(1:60, function(i) {                    # Loop over 60 frame numbers
  angle <- 2 * pi * (i-1) / 60                          # Calculate how much the whole galaxy should be rotated for this frame (0° → 354°)
  
  png(tmp <- tempfile(fileext = ".png"), width=1000, height=1000, bg="#0A0A1F")
  # temporary PNG file in the background (to be deleted autoamtically)
  
  par(mar=rep(0,4), bg="#0A0A1F")                        # Remove margins and set dark cosmic background
  plot(1, type="n", xlim=c(-5.2,5.2), ylim=c(-5.2,5.2), axes=F, xlab="", ylab="", asp=1)
  # Empty plot with square aspect ratio (asp=1) for better rotation
  
  x <- df$radius * cos(df$theta + angle)                # Convert polar → Cartesian coordinates, adding the rotation angle
  y <- df$radius * sin(df$theta + angle)                # Same for y
  
  points(x, y, pch=19, cex=df$point_size*5, col=add_alpha(species_colors[df$species], 0.25))
  # for big semi-transparent dots underneath → creates soft glow
  
  points(x, y, pch=19, cex=df$point_size*1.8, col=species_colors[df$species])
  # for smaller brighter dots on top → the actual crisp penguin dots
  
  text(0, 4.3, "Palmer Penguins\nSpiral Galaxy", col="white", cex=3, font=2)
  # title
  
  dev.off()                                             # Finish and close the PNG file

  image_read(tmp)                                       # Read the freshly made PNG into magick
})
# End of loop → frames now holds 60 beautiful images

cat("Combining into smooth spinning GIF...\n")
gif <- image_animate(image_join(frames), fps = 20)      # Joining all 60 frames and animate at 20 frames per second
image_write(gif, gif_final)                             # Saving the final animated GIF

cat("\nDONE! Your SPINNING penguin galaxy is on Desktop:\n")
cat(" PENGUIN_GALAXY_SPINNING_FINAL.gif\n")
shell.exec(desktop)                                     # this opens our Desktop folder

```

# Final Output

![PENGUIN_GALAXY_SPINNING_FINAL](https://github.com/user-attachments/assets/81cf706b-2590-43cd-b3a7-0342e304dbe4)
