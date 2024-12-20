
## Appendix A - Defining Sets {-}

# Defining Sets {-}

Each structure has a “gain per structure” value associated with it, which is a measure of the linear length of key habitat upstream until the next structure. Grouping structures into sets starts by taking the most downstream structure (e.g., structure A) in the stream and the next structure immediately upstream of it (e.g., structure B) and calculates the average gain per structure in that set (structure A+B/2). This process is repeated with sets increasing from two structures to five structures (e.g., structure A+B+C+D+E/5). The set with the highest average gain per structure is grouped together. Structures are left as individuals if adding a second structure reduces the average gain per structure (e.g., structure A > structure A+B/2). After this set is grouped, the algorithm moves to the next structure upstream of the set and repeats the process iteratively until all structures in the stream are in a set and the entire process repeats for each stream in the network. 

