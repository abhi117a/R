


library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps) #to get the USA map

library(data.table) #converting data to table


# Get a shape file of states in the US

usa.df <- map_data("state") #dataframe of US states
colnames(usa.df)[5] <- "State" #getting state names

# Get the data to be plotted

usa.dat <- read.table("clinton_chance_sep19_2016.csv", header = T, sep = ",")#reading csv file


# Merge the data with the shape file

usa.df <- join(usa.df, usa.dat, by = "State", type = "inner")

# Abbreviations of states and where thy should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations


p <- function(data, title) {
  ggp <- ggplot() + 
    #		Draw borders of states
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = Chance), color = "black") + 
    # 		Use shades of red for plotting; trans = "reverse" option 
    # 		makes the shades go from dark to light as the income share increases, 
    
    scale_fill_gradient2(low = "#fe6a59",mid ="white" ,high = "#2aa1ec", midpoint = 50,space = "Lab", guide="colorbar") + 
    #		Add legend
    theme_bw()  + labs(fill = "Hillary Clinton Winning Chances based on %" 
                       ,title = "Presidential Candidates Hilary Clinton VS Donald Trump", x="", y="")+ 
    #		Add state abbreviations		
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
  return(ggp)
}


# Save the map to a file to viewing (you can plot on the screen also, but it takes
# much longer that way. The ratio of US height to width is 1:9.)
p(usa.df, figure.title)



