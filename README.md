# directed-linchpin-centrality
This project extends the [linchpin centrality](https://github.com/mnemesure/linchpin_centrality), measure, originally described in [Nemesure et al. 2021](https://appliednetsci.springeropen.com/articles/10.1007/s41109-021-00400-8), by: 
  - Accommodating the direction of edges into the calculation, and 
  - Incorporating a different calculation into the numerator within weighted networks

Such modifications are helpful for capturing the nuances of directed and/or weighted networks that were otherwise addressed in the previous iteration.


## Folder 
`directed-linchpin-centrality.Rproj` - project that holds all code, data, and packages
`directed-linchpin.R` - new linchpin centrality code with the modifications outlined above
`test_network.R` - code that crosschecks new code against old code for undirected-unweighted networks
`Linchpin.Centrality.R` - original linchpin centrality code


## Directed Calculations


