library(gridExtra)
library(png)

# Read in the png files
img1 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_gonogo.png")
img2 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_visualsearch_propcorrect.png")
img3 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_taskswitching.png")
img4 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_trailmaking.png")
img5 <- readPNG("C:/Users/kamilako/OneDrive - York University/cannabis project (Mila)/img/IE_nback.png")

# Combine the images into one plot
grid.arrange(rasterGrob(img1), rasterGrob(img5), 
             rasterGrob(img3), rasterGrob(img4),
             rasterGrob(img2), ncol = 2)

