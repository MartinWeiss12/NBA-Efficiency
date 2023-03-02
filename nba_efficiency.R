library(png)
library(grid)
library(units)
library(imager)
library(readxl)
library(ggplot2)
# install.packages("ggimage")

# source: https://www.nba.com/stats/teams/advanced?dir=A&sort=NET_RATING

setwd("/Users/martinweiss/Documents/Programing/Virginia Tech/STAT 3204/R")
df <- read_excel("nba_efficiency.xlsx")

team <- df$TEAM
oRtg <- df$OFFRTG
dRtg <- df$DEFRTG
normORtg <-   (2 * (oRtg - min(oRtg)) / (max(oRtg) - min(oRtg)) - 1)  * 10
normDRtg <- (-(2 * (dRtg - min(dRtg)) / (max(dRtg) - min(dRtg)) - 1)) * 10

imageLogoPathList <- list()
for (i in 1:length(team)) {
  imageLogoPathList[[i]] <- paste0("images/", tolower(team[i]), ".png")
}

plot <- ggplot(data = NULL, aes(x = 0, y = 0)) + 
  xlim(-10, 10) +
  ylim(-10, 10) + 
  labs(x = "Normalized Offensive Rating", y = "Normalized Defensive Rating", title = "Larry O'Brian or Victor Wembanyama?", subtitle = "Efficiency in the NBA in the 2022 - 2023 Season (As of All-Star Weekend)") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"))

plot <- plot + geom_vline(xintercept = 0, colour = "black") + geom_hline(yintercept = 0, colour = "black")

for (i in 1:length(team)) {
  plot <- plot + annotation_custom(rasterGrob(readPNG(imageLogoPathList[[i]]), width = unit(1, "in"), height = unit(1, "in")), xmin = normORtg[[i]]-1, xmax = normORtg[[i]]+1, ymin = normDRtg[[i]]-1, ymax = normDRtg[[i]]+1)
}

plot
print("Plot Done")