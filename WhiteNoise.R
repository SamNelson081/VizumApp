generateStatic <- function(noise = 1) {
  #Create a 100x100 matrix
  x <- matrix(0, ncol = 100, nrow = 100)
  
  #Loop through all cells of the matrix
  for(i in 1:nrow(x)) {
    for(j in 1:ncol(x)) {
      #Generate a random value between 0 and 1
      rand <- round(runif(1), digits = 2)
      
      #If the random value is less than the noise threshold
      if(rand < noise) {
        #Fill current cell with a random grayscale colour
        col <- floor(runif(3, min=0, max=256))/256
        
        x[i,j] <- DescTools::ColToGrey(rgb(col[1], col[2], col[3]))
      } else {
        #Fill current cell with white
        x[i,j] <- DescTools::ColToGrey(rgb(1, 1, 1))
      }
    }
  }
  x
}

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

loop <- seq(0, 1, by=0.01)
for(i in 1:length(loop)) {
  if(loop[i] <= 0.25) {
    animation::ani.options('interval' = 1)
  } else if(loop[i] > 0.25 && loop[i] < 0.5) {
    animation::ani.options('interval' = 0.75)
  } else if(loop[i] >= 0.5 && loop[i] < 0.75) {
    animation::ani.options('interval' = 0.5)
  } else  {
    animation::ani.options('interval' = 0.25)
  }
  
  animation::saveGIF({
    for(j in 1:10) scales::show_col(generateStatic(noise=loop[i]), border=NA, labels = FALSE)
  }, movie.name = paste0(loop[i], ".gif"))
  
  magick::image_write(magick::image_crop(magick::image_read(paste0(loop[i], ".gif")), magick::geometry_area(444, 444, 18, 18)), path=paste0("www/", loop[i] ,".gif"))
}

