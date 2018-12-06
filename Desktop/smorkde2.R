###https://warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/density/ 

library(MASS)
 data = read.table("C:\\Users\\Kyle\\Documents\\M4Rmappractice\\top500_psi_phi_bio.tsv", header=TRUE, na.strings="=NA()")
 good.data <- data[!is.na(data$Phi) & !is.na(data$Psi), c("Phi","Psi","Type")] ###remove all data points NA
 rm(data) #removes data from the environment
 good.data[28:38,] #see some of the data
 
 #type is just a variable, this is splitting the data by type
generic <- good.data[good.data$Type=="Generic",c("Phi","Psi")]
prepro <- good.data[good.data$Type=="PrePRO",c("Phi","Psi")]
proline <- good.data[good.data$Type=="PRO",c("Phi","Psi")]
glycine <- good.data[good.data$Type=="GLY",c("Phi","Psi")]
 
 #kde2d takes long, lat, h = bandwidth (we want 3km, on a 30m grid, h=100??) , n = number of grid points, will depends on size of the area/30, 
# limits ate the limits of the grid c(xmin, xmax, ymin, ymax)
proline.density <- kde2d(proline$Phi,proline$Psi, n=361, lims=c(-180,180,-180,180), h=c(10,10)) ##h provides the smoothing factor

summary(proline.density)

sum(proline.density$z)
0.9671689
max(proline.density$z)
0.0004633823
min(proline.density$z)
0

filled.contour(proline.density$x, proline.density$y, proline.density$z, xlab=expression(phi), ylab=expression(psi),
               main="Proline", levels=c(0,0.00000123,0.00000726,1),
               xlim=c(-180,180), ylim=c(-180,180), asp=1,
               col=c('#FFFFFF','#C0FFC0','#80FF80'))


library(sm)
proline.density <- sm.density(proline, h=c(5,5), nbins=361,
                                xlim=c(-180,180), ylim=c(-180,180),
                                xlab="phi", ylab="psi", zlab="")
filled.contour(proline.density$estimate, xlab=expression(phi), ylab=expression(psi),
               main="Proline",
               x = proline.density$eval.points[,"xgrid"],
               y = proline.density$eval.points[,"ygrid"],
               xlim=c(-180,180), ylim=c(-180,180), asp=1,
               levels=c(0,0.00000068,0.00000573,1),
               col=c('#FFFFFF','#C0FFC0','#80FF80'))


proline$value<- runif(nrow(proline))*10000
###These look very similar - hde2d attempts to normalise its values to sum to 1

ggplot(proline) + aes(proline$Phi, proline$Psi, z = proline$value) + geom_bin2d(stat="bin2d")
###this is a heat map

library(mapproj)
##convert from longlat into cartesian coordinates -  i.e. map onto a grid?
mapproject(proline$Phi, proline$Psi, projection="", parameters=NULL, orientation=NULL)

map.grid(c(-100,50,-100,200), nx=9, ny=9, labels=TRUE, pretty=TRUE)







library(maps)
m <- map("usa", plot=FALSE)
map("usa", project="albers", par=c(39, 45))
map.grid(m)

# get unprojected world limits
m <- map('world', plot=FALSE)

# center on NYC
map('world', proj='azequalarea', orient=c(41, -74, 0))
map.grid(m, col=2)
points(mapproject(list(y=41, x=-74)), col=3, pch="x", cex=2)

map('world', proj='orth', orient=c(41, -74, 0))
map.grid(m, col=2, nx=6, ny=5, label=FALSE, lty=2)
points(mapproject(list(y=41, x=-74)), col=3, pch="x", cex=2)

# center on Auckland
map('world', proj='orth', orient=c(-36.92, 174.6, 0))
map.grid(m, col=2, label=FALSE, lty=2)
points(mapproject(list(y=-36.92, x=174.6)), col=3, pch="x", cex=2)

m <- map('nz')
# center on Auckland
map('nz', proj='azequalarea', orient=c(-36.92, 174.6, 0))
points(mapproject(list(y=-36.92, x=174.6)), col=3, pch="x", cex=2)
map.grid(m, col=2)

