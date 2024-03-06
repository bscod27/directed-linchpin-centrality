linchpin_centrality <- function(g, id = 'name', attr = 'spec', type1 = NULL, type2 = NULL) {
  require(igraph)
  
  # decipher if graph is directed and/or weighted
  directed <- is.directed(g)
  weighted <- is.weighted(g)
  
  # defensive programming
  if (directed & (is.null(type1) | is.null(type2))) {
    stop("Warning: Graph is directed, but 'type1' and 'type2' arguments not provided")
  } else if (!directed) {
    type1 <- 'all'; type2 <- 'all'
  }
  
  # store first-order degree/strength into dataframe
  deg.stats <- data.frame(
    name=vertex_attr(g)[id], spec=vertex_attr(g)[attr],
    degree = degree(g, mode = type1),
    strength = strength(g, mode = type1),
    row.names = NULL
  )
  
  # loop through each node
  linchpin <- c()
  counter <- 0
  for (i in deg.stats$name) {
    # store focal specialty and calculate denom
    if (counter %% 500 == 0) {print(paste0(counter, '/', nrow(deg.stats)))}
    focal_spec <- deg.stats[deg.stats$name==i, attr]
    num <- 0
    denom <- ifelse(weighted,deg.stats[deg.stats$name==i, 'strength'],deg.stats[deg.stats$name==i, 'degree'])
  
    # check for isolates (denom == 0)
    if (denom == 0) {
      linchpin <- c(linchpin, NA) # default to NA due to zero division 
      counter <- counter + 1
      next
    }
  
    # loop through node i's neighbors' connections (if denom > 0)
    for (j in unique(neighbors(g, i, mode = type1))) { 
      # extract name of neighbor and second-order ties
      nm <- deg.stats$name[j]
      temp <- deg.stats[unique(neighbors(g, nm, mode = type2)), ] # consider second-order directionality
      temp <- temp[temp$name != i, ]
  
      # if focal node i's specialty is NOT in node j's neighbors specialties
      if (!(focal_spec %in% temp$spec)) {
  
        if (directed & weighted) { # directed and weighted case
          mat <- get.edgelist(g, names = TRUE)
          idx <- ifelse(type == 'in', which(mat[, 1]==nm & mat[, 2] == i), which(mat[, 1]==i & mat[, 2] == nm))
          num <- num + E(g)$weight[idx]
  
        } else if (!directed & weighted) { # undirected and weighted case
          idx <- get.edge.ids(g, vp = c(nm, i))
          num <- num + E(g)$weight[idx]
  
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
