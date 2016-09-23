# Initialize environment
library("xlsxjars", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("rJava", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("circlize", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
setwd("~/Desktop/chord_diagram_Cr_V")

###########################################################

# Load paragentic modes from para_modes_colored.xlsx 
#   - all variables should be treated as strings: colClasses = "character"
#   - para modes are shared by both Cr and V minerals
para.modes <- read.xlsx("para_modes_colored.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE, colClasses = "character")

# Load data on Cr and V minerals' association with their para modes
V.modes <- read.csv("vanadium_modes.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = "character")
Cr.modes <- read.csv("chromium_modes.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = "character")

# Read in vanadium locations file. Use read.csv since read.xlsx can stuck forever (file too large)
V.locations <- read.csv("vanadium_locations.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(V.locations) <- V.locations[1, ]
V.locations <- V.locations[-1, ]

# Read in chromium locations file. Use read.csv since read.xlsx can stuck forever (file too large)
Cr.locations <- read.csv("chromium_locations.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(Cr.locations) <- Cr.locations[1, ]
Cr.locations <- Cr.locations[-1, ]
# Also generate a chromium location dataset without chromite:
Cr.locations.no.chromite <- Cr.locations
Cr.locations.no.chromite$Chromite <- NULL

# Verify that lists of minerals are consistent among corresponding files (IMPORTANT!)
colnames(V.locations) == V.modes[, 1]
colnames(Cr.locations) == Cr.modes[, 1]
# Or verify by
prod(colnames(V.locations) == V.modes[, 1]) == 1
prod(colnames(Cr.locations) == Cr.modes[, 1]) == 1

################################################################################

############################
# Chord diagram of vanadiums
############################

# Set variables (only modify variable settings)
mineral.modes <- V.modes
mineral.locations <- V.locations

# Calculate adjacency matrix and plotting (no change needed)
from.vec <- character()
to.vec <- character()
value.vec <- numeric()
color.vec <- character()
color.code.vec <- character()
num.minerals <- dim(mineral.locations)[2]
for (i in 1:(num.minerals - 1)) {
  for (j in (i + 1):num.minerals) {
    value <- sum(as.logical(as.numeric(mineral.locations[, i]) * as.numeric(mineral.locations[, j])))
    if ( value > 0) {
      from <- colnames(mineral.locations)[i]
      to <- colnames(mineral.locations)[j]
      color.code <- mineral.modes[which(mineral.modes$mineral.name == from), "para.mode.category"][1]
      color <- para.modes[which(para.modes$label == color.code), "color"]
      from.vec <- c(from.vec, from)
      to.vec <- c(to.vec, to)
      value.vec <- c(value.vec, value)
      color.vec <- c(color.vec, color)
      color.code.vec <- c(color.code.vec, color.code)
    }
  }
}
adjacency.list <- data.frame(from = from.vec, to = to.vec, value = value.vec, color = color.vec, color.code = color.code.vec, stringsAsFactors = FALSE)
# Create grid colors according to existing minerals in the adjacency list
grids <- union(adjacency.list$from, adjacency.list$to)
grid.colors <- data.frame(mineral = grids, color = rep(NA, length(grids)), stringsAsFactors = FALSE)
for (i in 1:length(grids)) {
  color.code <- mineral.modes[which(mineral.modes$mineral.name == grid.colors[i, "mineral"]), "para.mode.category"][1]
  grid.colors[i, "color"] <- para.modes[which(para.modes$label == color.code), "color"]
}
# Sort grid.colors by color for ordering of grids
grid.colors.sorted <- grid.colors[order(grid.colors$color), ]
# Generate named vector for grid colors
grid.colors.named.vector <- grid.colors$color
names(grid.colors.named.vector) <- grid.colors$mineral
# Plot
chordDiagramFromDataFrame(adjacency.list, order = grid.colors.sorted$mineral, transparency = 0.2,
                          grid.col = grid.colors.named.vector, col = adjacency.list$color, 
                          annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5)) } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", niceFacing = TRUE, adj = c(0.5, 0))
                }
}, bg.border = NA)
circos.clear()
##


############################
# Chord diagram of chromiums
############################

# Set variables (only modify variable settings)
mineral.modes <- Cr.modes
mineral.locations <- Cr.locations

# Calculate adjacency matrix and plotting (no change needed)
from.vec <- character()
to.vec <- character()
value.vec <- numeric()
color.vec <- character()
color.code.vec <- character()
num.minerals <- dim(mineral.locations)[2]
for (i in 1:(num.minerals - 1)) {
  for (j in (i + 1):num.minerals) {
    value <- sum(as.logical(as.numeric(mineral.locations[, i]) * as.numeric(mineral.locations[, j])))
    if ( value > 0) {
      from <- colnames(mineral.locations)[i]
      to <- colnames(mineral.locations)[j]
      color.code <- mineral.modes[which(mineral.modes$mineral.name == from), "para.mode.category"][1]
      color <- para.modes[which(para.modes$label == color.code), "color"]
      from.vec <- c(from.vec, from)
      to.vec <- c(to.vec, to)
      value.vec <- c(value.vec, value)
      color.vec <- c(color.vec, color)
      color.code.vec <- c(color.code.vec, color.code)
    }
  }
}
adjacency.list <- data.frame(from = from.vec, to = to.vec, value = value.vec, color = color.vec, color.code = color.code.vec, stringsAsFactors = FALSE)
# Create grid colors according to existing minerals in the adjacency list
grids <- union(adjacency.list$from, adjacency.list$to)
grid.colors <- data.frame(mineral = grids, color = rep(NA, length(grids)), stringsAsFactors = FALSE)
for (i in 1:length(grids)) {
  color.code <- mineral.modes[which(mineral.modes$mineral.name == grid.colors[i, "mineral"]), "para.mode.category"][1]
  grid.colors[i, "color"] <- para.modes[which(para.modes$label == color.code), "color"]
}
# Sort grid.colors by color for ordering of grids
grid.colors.sorted <- grid.colors[order(grid.colors$color), ]
# Generate named vector for grid colors
grid.colors.named.vector <- grid.colors$color
names(grid.colors.named.vector) <- grid.colors$mineral
# Plot
chordDiagramFromDataFrame(adjacency.list, order = grid.colors.sorted$mineral, transparency = 0.5,
                          grid.col = grid.colors.named.vector, col = adjacency.list$color, 
                          annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5)) } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", niceFacing = TRUE, adj = c(0.5, 0))
                }
}, bg.border = NA)
circos.clear()
#
######################

############################
# Chord diagram of chromiums WITHOUT chromite
############################

# Set variables (only modify variable settings)
mineral.modes <- Cr.modes
mineral.locations <- Cr.locations.no.chromite

# Calculate adjacency matrix and plotting (no change needed)
from.vec <- character()
to.vec <- character()
value.vec <- numeric()
color.vec <- character()
color.code.vec <- character()
num.minerals <- dim(mineral.locations)[2]
for (i in 1:(num.minerals - 1)) {
  for (j in (i + 1):num.minerals) {
    value <- sum(as.logical(as.numeric(mineral.locations[, i]) * as.numeric(mineral.locations[, j])))
    if ( value > 0) {
      from <- colnames(mineral.locations)[i]
      to <- colnames(mineral.locations)[j]
      color.code <- mineral.modes[which(mineral.modes$mineral.name == from), "para.mode.category"][1]
      color <- para.modes[which(para.modes$label == color.code), "color"]
      from.vec <- c(from.vec, from)
      to.vec <- c(to.vec, to)
      value.vec <- c(value.vec, value)
      color.vec <- c(color.vec, color)
      color.code.vec <- c(color.code.vec, color.code)
    }
  }
}
adjacency.list <- data.frame(from = from.vec, to = to.vec, value = value.vec, color = color.vec, color.code = color.code.vec, stringsAsFactors = FALSE)
# Create grid colors according to existing minerals in the adjacency list
grids <- union(adjacency.list$from, adjacency.list$to)
grid.colors <- data.frame(mineral = grids, color = rep(NA, length(grids)), stringsAsFactors = FALSE)
for (i in 1:length(grids)) {
  color.code <- mineral.modes[which(mineral.modes$mineral.name == grid.colors[i, "mineral"]), "para.mode.category"][1]
  grid.colors[i, "color"] <- para.modes[which(para.modes$label == color.code), "color"]
}
# Sort grid.colors by color for ordering of grids
grid.colors.sorted <- grid.colors[order(grid.colors$color), ]
# Generate named vector for grid colors
grid.colors.named.vector <- grid.colors$color
names(grid.colors.named.vector) <- grid.colors$mineral
# Plot
chordDiagramFromDataFrame(adjacency.list, order = grid.colors.sorted$mineral, transparency = 0.5,
                          grid.col = grid.colors.named.vector, col = adjacency.list$color, 
                          annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5)) } else {
                  circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", niceFacing = TRUE, adj = c(0.5, 0))
                }
}, bg.border = NA)
circos.clear()
#
######################





