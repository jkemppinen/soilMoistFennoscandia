library(tidyverse)
library(distantia)
library(ggplot2)
library(viridis)
library(kableExtra)
library(qgraph)
library(tidyr)
library(lubridate)

# bring logger soil moisture files
your_sm  <- read.csv ("data/all_data_daily.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
                              RAS = "a",
                              KIL = "b",
                              VAR = "c",
                              TII = "d",
                              PIS = "e",
                              HYY = "f",
                              KAR = "g")

your_sm %>% mutate (date = as_date(date))-> your_sm

#kilpisjärvi data
sequenceA <- your_sm %>% 
  filter(area %in% c("b")) %>% 
  filter(year(date) %in% c(2020)) %>%
  filter(month(date) %in% c(6, 7, 8, 9)) %>% 
  pivot_wider(date, names_from = id_code, values_from = moist_mean) %>% 
  select(-date)

#värriö data
sequenceB <- your_sm %>% 
  filter(area %in% c("c")) %>% 
  filter(year(date) %in% c(2020)) %>%
  filter(month(date) %in% c(6, 7, 8, 9)) %>% 
  pivot_wider(date, names_from = id_code, values_from = moist_mean) %>% 
  select(-date)

#checking the function help-file.
help(prepareSequences)

#preparing sequences
AB.sequences <- prepareSequences(
  sequence.A = sequenceA,
  sequence.A.name = "A",
  sequence.B = sequenceB,
  sequence.B.name = "B",
  merge.mode = "complete",
  if.empty.cases = "zero",
  transformation = "none"
)

#computing distance matrix
AB.distance.matrix <- distanceMatrix(
  sequences = AB.sequences,
  method = "euclidean"
)

#plotting distance matrix
plotMatrix(
  distance.matrix = AB.distance.matrix,
  color.palette = "viridis",
  margins = rep(4,4))

#ORTHOGONAL SEARCH
#computing least-cost matrix
AB.least.cost.matrix <- leastCostMatrix(
  distance.matrix = AB.distance.matrix,
  diagonal = FALSE
)

#extracting least-cost path
AB.least.cost.path <- leastCostPath(
  distance.matrix = AB.distance.matrix,
  least.cost.matrix = AB.least.cost.matrix,
  diagonal = FALSE
)


#DIAGONAL SEARCH
#computing least-cost matrix
AB.least.cost.matrix.diag <- leastCostMatrix(
  distance.matrix = AB.distance.matrix,
  diagonal = TRUE
)

#extracting least-cost path
AB.least.cost.path.diag <- leastCostPath(
  distance.matrix = AB.distance.matrix,
  least.cost.matrix = AB.least.cost.matrix.diag,
  diagonal = TRUE
)

#plotting solutions
plotMatrix(
  distance.matrix = list(
    'A|B' = AB.least.cost.matrix[[1]], 
    'A|B' = AB.least.cost.matrix.diag[[1]]
  ),
  least.cost.path = list(
    'A|B' = AB.least.cost.path[[1]], 
    'A|B' = AB.least.cost.path.diag[[1]]
  ),
  color.palette = "viridis",
  margin = rep(4,4),
  plot.rows = 1,
  plot.columns = 2
)

#orthogonal solution
AB.between <- leastCost(
  least.cost.path = AB.least.cost.path
)

#diagonal solution
AB.between.diag <- leastCost(
  least.cost.path = AB.least.cost.path.diag
)

#ORTHOGONAL SOLUTION
#removing blocks from least cost path
AB.least.cost.path.nb <- leastCostPathNoBlocks(
  least.cost.path = AB.least.cost.path
)

#computing AB.between again
AB.between.nb <- leastCost(
  least.cost.path = AB.least.cost.path.nb
)


#DIAGONAL SOLUTION
#removing blocks
AB.least.cost.path.diag.nb <- leastCostPathNoBlocks(
  least.cost.path = AB.least.cost.path.diag
)

#diagonal solution without blocks
AB.between.diag.nb <- leastCost(
  least.cost.path = AB.least.cost.path.diag.nb
)

#changing names of the selected solutions
AB.least.cost.path <- AB.least.cost.path.diag.nb
AB.between <- AB.between.diag.nb

#removing unneeded objects
rm(AB.between.diag, AB.between.diag.nb, AB.between.nb, AB.distance.matrix, AB.least.cost.matrix, AB.least.cost.matrix.diag, AB.least.cost.path.diag, AB.least.cost.path.diag.nb, AB.least.cost.path.nb, sequenceA, sequenceB)

AB.within <- autoSum(
  sequences = AB.sequences,
  least.cost.path = AB.least.cost.path,
  method = "euclidean"
)
AB.within
#$`A|B`
#[1] 8441.636

AB.psi <- psi(
  least.cost = AB.between,
  autosum = AB.within
)
AB.psi[[1]] <- AB.psi[[1]] + 1

#to dataframe
AB.psi.dataframe <- formatPsi(
  psi.values = AB.psi,
  to = "dataframe")

#checking the help file
help(workflowPsi)

#computing psi for A and B
AB.psi <- workflowPsi(
  sequences = AB.sequences,
  grouping.column = "id",
  method = "euclidean",
  format = "list",
  diagonal = TRUE,
  ignore.blocks = TRUE
)
AB.psi
$`A|B`
[1] 10.96817

