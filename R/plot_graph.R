#' Plot Annotations
#' @description Plots the annotation graph.
#' @param con The RNeo4j connection.
#'
plot_graph <- function(con) {
  nodes <- cypher(con, "MATCH (n1)-[r1]-(n2)
                        RETURN DISTINCT ID(n1) AS id, LABELS(n1) AS type, '24' AS size")

  edges <- cypher(con, "MATCH (n1)-[r1]-(n2)
                  RETURN ID(n1) AS source, ID(n2) AS target, type(r1) AS type")

  fd_data <- data.frame(source = (0:(nrow(nodes)-1))[match(edges$source, nodes$id)],
                        target = (0:(nrow(nodes)-1))[match(edges$target, nodes$id)],
                        rel    = edges$type)

  forceNetwork(Links = fd_data,
               Nodes = nodes,
               Source = 'source',
               Target = 'target',
               Value = 'rel',
               NodeID = 'type',
               Group = 'type',
               Nodesize = 'size',
               fontSize = 14,
               height = 150,
               width = 300,
               opacity = 1,
               colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
               legend = TRUE)
}
