require(tidyverse)
require(igraph)
source('Linchpin.Centrality.R') # Matt's function
source('directed_linchpin.R') # my function
set.seed(123)



##### Load in data and form networks #####
edges <- read.csv('../../03. Research Rotations/01. Moen Lab/01. Practice Data/Prov_HRR_edgelist.csv') 
nodes <- read.csv('../../03. Research Rotations/01. Moen Lab/01. Practice Data/Prov_HRR_key.csv') %>% rename(spec = spspec)

# undirected
g.undirected.unweighted <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
g.undirected.weighted <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
E(g.undirected.weighted)$weight <- round(runif(nrow(edges), 1, 10))

# directed
g.directed.unweighted <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
g.directed.weighted <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
E(g.directed.weighted)$weight <- round(runif(nrow(edges), 1, 10))



##### Test function against Matt's #####
# my code
o1a <- linchpin_centrality(g.undirected.unweighted, specs = c('rad', 'surg')) # should match Matt's code
o2a <- linchpin_centrality(g.undirected.weighted, specs = c('rad', 'surg'))

# Matt's Code
o1b <- Linchpin.Centrality(
  links = edges, characteristics = nodes, label_column_name = 'spec', 
  char_subset = c('rad', 'surg'),
  weighted = FALSE
)[[3]]$linkage

edges$weight <- E(g.undirected.weighted)$weight
o2b <- Linchpin.Centrality(
  links = edges, characteristics = nodes, label_column_name = 'spec', 
  char_subset = c('rad', 'surg'),
  weighted = TRUE
)[[3]]$linkage


# checks
table(o1a == o1b)
table(o2a == o2b)

# summaries
summary(o1a) # my code returns more NAs
summary(o1b) # Matt's code returns more zeroes

# looks like Matt's function returns zeroes for specialties NOT under consideration, which is what we don't want
data.frame(spec=V(g.undirected.unweighted)$spec, o1a, o1b) %>% 
  group_by(spec) %>% 
  summarize(
    sum(!is.na(o1a)),
    sum(!is.na(o1b))
    ) 


# # directed, in
in1 <- linchpin_centrality(g.directed.unweighted, type = 'in', specs = c('rad', 'surg'))
in2 <- linchpin_centrality(g.directed.weighted, type = 'in', specs = c('rad', 'surg'))
hist(in1)
hist(in2)

# directed, out
out1 <- linchpin_centrality(g.directed.unweighted, type = 'out', specs = c('rad', 'surg'))
out2 <- linchpin_centrality(g.directed.weighted, type = 'out', specs = c('rad', 'surg'))
hist(out1)
hist(out2)


# output
out <- nodes %>%
  cbind(o1a, o2a, in1, in2, out1, out2) %>%
  rename_with(., ~c('npi', 'spec', 'Und_Unw', 'Und_Wht', 'In_Unw', 'In_Wht', 'Out_Unw', 'Out_Wht'))

out %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  group_by(spec) %>%
  summarize(
    mean(Und_Unw), mean(Und_Wht),
    mean(In_Unw), mean(In_Wht),
    mean(Out_Unw), mean(Out_Wht)
  ) %>%
  arrange(spec)

sapply(out, function(i) sum(is.na(i)))/nrow(out)



##### Interrogate the correlations #####
df <- out %>% mutate_if(is.numeric, ~replace_na(., 0)) # zeroed out 
df2 <- out %>% filter(complete.cases(.)) # complete cases

# zeroed out - unweighted
mat <- matrix(NA, 3, 3)
colnames(mat) <- (df %>% select(contains('_Unw')) %>% colnames)
rownames(mat) <- (df %>% select(contains('_Unw')) %>% colnames)
for (i in (df %>% select(contains('_Unw')) %>% colnames)) {
  for (j in (df %>% select(contains('_Unw')) %>% colnames)) {
    mat[i,j] <- cor(df[,i], df[,j])
  }
}
mat
# write.csv(mat, 'corr_zeroed_unw.csv')

# zeroed out - weighted
mat <- matrix(NA, 3, 3)
colnames(mat) <- (df %>% select(contains('_Wht')) %>% colnames)
rownames(mat) <- (df %>% select(contains('_Wht')) %>% colnames)
for (i in (df %>% select(contains('_Wht')) %>% colnames)) {
  for (j in (df %>% select(contains('_Wht')) %>% colnames)) {
    mat[i,j] <- cor(df[,i], df[,j])
  }
}
mat
# write.csv(mat, 'corr_zeroed_wht.csv')

# completed - unweighted
mat <- matrix(NA, 3, 3)
colnames(mat) <- (df2 %>% select(contains('_Unw')) %>% colnames)
rownames(mat) <- (df2 %>% select(contains('_Unw')) %>% colnames)
for (i in (df2 %>% select(contains('_Unw')) %>% colnames)) {
  for (j in (df2 %>% select(contains('_Unw')) %>% colnames)) {
    mat[i,j] <- cor(df2[,i], df2[,j])
  }
}
mat
# write.csv(mat, 'corr_completed_unw.csv')

# completed - weighted
mat <- matrix(NA, 3, 3)
colnames(mat) <- (df2 %>% select(contains('_Wht')) %>% colnames)
rownames(mat) <- (df2 %>% select(contains('_Wht')) %>% colnames)
for (i in (df2 %>% select(contains('_Wht')) %>% colnames)) {
  for (j in (df2 %>% select(contains('_Wht')) %>% colnames)) {
    mat[i,j] <- cor(df2[,i], df2[,j])
  }
}
mat
# write.csv(mat, 'corr_completed_wht.csv')



# ##### Erika's question #####
# specialties <- c('radonc', 'medonc', 'surgeon')
# g <- erdos.renyi.game(25, p=.1, directed = FALSE, loops = FALSE)
# V(g)$name <- 1:25
# V(g)$spec <- sample(specialties, 25, replace = TRUE)
# plot(g, vertex.color = factor(V(g)$spec))
# 
# edges <- data.frame(get.edgelist(g))
# colnames(edges) <- c('ID_1', 'ID_2')
# nodes <- data.frame(vertex_attr(g)) %>% rename(ID_1 = name)
# 
# x <- linchpin_centrality(g)
# y <- Linchpin.Centrality(
#     links=edges, characteristics=nodes, label_column_name='spec', char_subset='empty', weighted=FALSE
#   )[[3]]$linkage
# 
# plot(g, layout = layout.fruchterman.reingold, vertex.color = factor(V(g)$spec))  
# data.frame(node=1:25, bruno=x, matt=y) %>% head(14)
