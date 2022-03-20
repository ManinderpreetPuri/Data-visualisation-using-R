#Task- 2 a.

#Loading libraries
  library(ggplot2)
  library(gplots) 
  library(RColorBrewer)
  library(dplyr)

#Reading data  
  grandslam <- read.csv(file = 'grand_slam_data.csv')
  
#Filtering top winners in all four tournaments  
  grandslam_chart = grandslam %>% group_by(winner) %>% summarise(Number=n()) %>% arrange(desc(Number))
  grandslam_chart$winner <- factor(grandslam_chart$winner, levels = grandslam_chart$winner[order(grandslam_chart$Number)])
  top_grandslam = grandslam_chart %>% filter(Number >= 4)
  
  grandslam_trn = grandslam %>% filter(winner %in% top_grandslam$winner) %>% group_by(tournament, winner) %>% summarise(Number=n()) %>% arrange(desc(Number))
  grandslam_trn$Number <- factor(grandslam_trn$Number)
  grandslam_trn <- head(grandslam_trn , 20)
  
#Plotting data  
  ggplot(data=grandslam_trn , aes(x=winner, y=Number, fill=Number)) +
    geom_bar(stat='identity') + coord_flip() + guides(fill=FALSE) + scale_y_discrete() +
    facet_grid(. ~ grandslam_trn$tournament) +
    labs(title= "Comparison of top 20 players in all 4 tournaments", x = "Winner Players", y= "Number of wins") 
  
#Task=2 b.
  
 #Splitting/filtering data for top 40 most	recent	tournaments 
  recent <- 1:40
  runnerups <- grandslam[recent, c("winner", "runner_up")]
  runner_up_players <- unique(c(runnerups[,1], runnerups[,2]))
  
  grid1 <- matrix(0, nrow=length(runner_up_players),
                  ncol=length(runner_up_players))
  colnames(grid1) <- runner_up_players
  rownames(grid1) <- runner_up_players
  
  for(i in 1:nrow(runnerups)) {
    winner <- runnerups[i, "winner"]
    runner_up <- runnerups[i, "runner_up"]
    a <- which(rownames(grid1) == winner)
    b <- which(colnames(grid1) == runner_up)
    
    grid1[a,b] <- grid1[a,b] + 1
    grid1[b,a] <- grid1[b,a] + 1
  }
  
  diag(grid1) <- NA
  
  
# Using heatmaps to plot data
  coloring <- colorRampPalette(c("green", "yellow", "red"))(n = 299)
  color_prp = c(seq(0, 0.99, length=100),  # for green
                 seq(1, 5, length=100),     # for yellow
                 seq(5.01, 10, length=100)) # for red
  heatmap.2(grid1,
            cellnote = grid1,  # same data set for cell labels
            main = "Tennis Grand Slam Champions - winners and  runner-ups", # heat map title
            notecol = "black",      # change font color of cell labels to black
            density.info = "none",  # turns off density plot inside color legend
            trace = "none",         # turns off trace lines inside the heat map
            margins = c(12,9),     # widens margins around plot
            col= coloring,       # use on color palette defined earlier
            breaks = color_prp,    # enable color transition at specified limits
            dendrogram = "none",     # only draw a row dendrogram
            Colv = "NA")
  
  
  

  
  
  
  
  
