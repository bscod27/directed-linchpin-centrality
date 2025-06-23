##### Load in data and form networks #####
library(tidyverse)
library(igraph)

edges <- read.csv('../../03. Research Rotations/01. Moen Lab/01. Practice Data/Prov_HRR_edgelist.csv') 
nodes <- read.csv('../../03. Research Rotations/01. Moen Lab/01. Practice Data/Prov_HRR_key.csv') %>% rename(spec = spspec)

g.directed.weighted <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
E(g.directed.weighted)$weight <- round(runif(nrow(edges), 1, 10))

g <- g.directed.weighted
id = 'name'
attr = 'spec'
type = 'in'
specs = c('rad', 'surgery')

linchpin_centrality <- function(g, id = 'name', attr = 'spec', specs = NULL, type = NULL) {
  
  ##### Docstring #####
  #' Calculate linchpin score on each node in an igraph network
  #' 
  #' Arguments: 
  #'   g: An igraph network object
  #'   id: The vertex attribute specifying the node identifier
  #'   attr: The vertex attribute specifying the classification of each node
  #'   specs: A vector containing the subset of classifications to consider during linchpin score calculation
  #'   type: The input that controls the directionality of linchpin that is calculated
  #' 
  #' Returns: 
  #'   A vector of linchpin score values which correspond to the ordering of nodes in the inputted igraph network object
  #'   
  #' Examples: 
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc')) # calculates undirected linchpin scores
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc'), type='in') # calculates directed in-linchpin scores
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc'), type='out') # calculates directed out-linchpin scores
  
  ##### Code #####
  
  # load required packages
  require(igraph)
  
  # decipher if graph is directed and/or weighted
  directed <- is.directed(g)
  weighted <- is.weighted(g)
  
  # defensive programming
  if (directed & (is.null(type))) {
    stop("Warning: Graph is directed, but 'type' argument not provided")
  } else if (!directed) {
    type <- 'all'
  }
  
  # store first-order degree/strength into dataframe
  deg.stats <- data.frame(
    name = vertex_attr(g)[id], spec = vertex_attr(g)[attr],
    degree = degree(g, mode = type),
    strength = strength(g, mode = type),
    row.names = NULL
  )
  
  # loop through each node
  linchpin <- c()
  counter <- 0
  for (i in deg.stats$name) {
    if (counter %% 500 == 0) {print(paste0(counter, '/', nrow(deg.stats)))}
    
    # store focal specialty 
    focal_spec <- deg.stats[deg.stats$name==i, attr]
    
    # check if focal_spec in specs and specs not null
    if (!focal_spec %in% specs & !is.null(specs)) {
      linchpin <- c(linchpin, NA)
      counter <- counter + 1
      next
    }
    
    
    # calculate denominator and check for isolates (denom == 0)
    denom <- ifelse(weighted, deg.stats[deg.stats$name==i, 'strength'], deg.stats[deg.stats$name==i, 'degree'])
  
    if (denom == 0) {
      linchpin <- c(linchpin, NA) # default to NA due to zero division 
      counter <- counter + 1
      next
    }
  
    # calculate numerator by looping  node i's neighbors' connections
    num <- 0
    for (j in unique(neighbors(g, i, mode = type))) { 
      # extract name of neighbor and second-order ties
      nm <- deg.stats$name[j]
      if (type == 'in') {
        type2 <- 'out'
      } else if (type == 'out') {
        type2 <- 'in'
      } else {
        type2 <- 'all'
      }
      temp <- deg.stats[unique(neighbors(g, nm, mode = type2)), ] # consider second-order directionality
      temp <- temp[temp$name != i, ] # don't consider connections with focal node
      
  
      # if focal node i's specialty is NOT in node j's neighbors specialties
      if (!(focal_spec %in% temp$spec)) {
  
        if (directed & weighted) { # directed and weighted case
          mat <- get.edgelist(g, names = TRUE)
          idx <- ifelse(type == 'in', which(mat[, 1] == nm & mat[, 2] == i), which(mat[, 1] == i & mat[, 2] == nm))
          num <- num + as.numeric(E(g)$weight[idx])
  
        } else if (!directed & weighted) { # undirected and weighted case
          idx <- get.edge.ids(g, vp = c(nm, i))
          num <- num + as.numeric(E(g)$weight[idx])
  
        } else { # all unweighted cases
          num <- num + 1
        }
      }
    }
    
    # calculate linchpin score and store
    linchpin <- c(linchpin, num/denom)
    counter <- counter + 1
  }

  # return output 
  return(linchpin)
}
