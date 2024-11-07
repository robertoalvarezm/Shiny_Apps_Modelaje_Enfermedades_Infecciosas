# Set the size of the grid
grid.size <- 100

# Create a matrix to represent the grid
grid <- matrix(rep(1,grid.size*grid.size),nrow=grid.size, ncol=grid.size)

# Set the initial values of the grid
#grid[1,1] <- "S"
grid[grid.size/2,grid.size/2] <- 2

# Set the probability of an individual becoming infected
infection.probability <- 0.15

# Set the probability of an infected individual recovering
recovery.probability <- 0.15

# Set the maximum number of iterations
max.iterations <- 1000
out<-data.frame(time=1:max.iterations,
                Susceptible=rep(0,max.iterations),
                Infected=rep(0,max.iterations),
                Recovered=rep(0,max.iterations))
# Run the cellular automata
for (iteration in 1:max.iterations) {
  # Create a new matrix to store the updated grid
  new.grid <- matrix(rep(1,grid.size*grid.size), nrow=grid.size, ncol=grid.size)
  
  # Loop over each cell in the grid
  for (i in 1:grid.size) {
    for (j in 1:grid.size) {
      # If the cell is currently susceptible
      if (grid[i,j] == 1) {
        # Check the surrounding cells to see if any are infected
        infected.neighbors <- 0
        if (i > 1 && grid[i-1,j] == 2) infected.neighbors <- infected.neighbors + 1
        if (i < grid.size && grid[i+1,j] == 2) infected.neighbors <- infected.neighbors + 1
        if (j > 1 && grid[i,j-1] == 2) infected.neighbors <- infected.neighbors + 1
        if (j < grid.size && grid[i,j+1] == 2) infected.neighbors <- infected.neighbors + 1
        
        # If at least one neighbor is infected, there is a chance the cell will become infected
        if (infected.neighbors > 0 && runif(1) < infection.probability) {
          new.grid[i,j] <- 2
        } else {
          new.grid[i,j] <- 1
        }
      }
      # If the cell is currently infected
      if (grid[i,j] == 2) {
        # There is a chance the cell will recover
        if (runif(1) < recovery.probability) {
          new.grid[i,j] <- 3
        } else {
          new.grid[i,j] <- 2
        }
      }
    }
  }
  
  # Update the grid with the new values
   grid <- new.grid
   print(table(grid)[[2]])
   if(any(grid==3)){
   out[iteration,2]<-table(grid)[[1]]
   out[iteration,3]<- table(grid)[[2]]
  out[iteration,4] <-table(grid)[[3]]}else{
  out[iteration,2]<-table(grid)[[1]]
  out[iteration,3]<- table(grid)[[2]]
   out[iteration,4] <-0  
   }
}


library(plot.matrix)
library(ggplot2)
library(viridis)
pdf("03_Results/Plots/Resultado.pdf")
plot(grid,col=viridis(3),fmt.key="%.0f")
dev.off()


x1=melt(grid)
names(x1)=c("x","y","state")
x1$state=factor(x1$state==1)
levels(x1$state)=c("Susceptible","Infected","Recovered")

qplot(x, y, fill=state, data=x1,geom='tile')
 
