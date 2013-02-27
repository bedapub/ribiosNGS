list2mat <- function(list) {
  listStr <- lapply(list, as.character)
  t(.Call("list2mat", listStr))
}
