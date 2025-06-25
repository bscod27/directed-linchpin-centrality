# directed-linchpin-centrality
This project extends the [linchpin centrality](https://github.com/mnemesure/linchpin_centrality) measure - originally described in [Nemesure et al. 2021](https://appliednetsci.springeropen.com/articles/10.1007/s41109-021-00400-8) - by accounting for:
  - The direction of first- and second-order ties, and 
  - The weighting of second-order ties

Such modifications are helpful for capturing the nuances of directed and/or weighted networks that were otherwise unaddressed in the previous iteration.


## Scripts
  - `directed-linchpin-centrality.Rproj` - project that holds all code, data, and packages/dependencies
  - `directed-linchpin.R` - new linchpin centrality code with the modifications outlined above
  - `Linchpin.Centrality.R` - original linchpin centrality code
  - `test_network.R` - code that crosschecks new code against old code for undirected-unweighted networks


## Examples
![](illustration.png)
