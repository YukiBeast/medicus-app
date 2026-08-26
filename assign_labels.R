assign_labels <- function(code, label = labels) {

  to_assign <- vapply(code,
                      function(x) gsub("^.*: ", "", labels[[x]]),
                      character(1))
  
  return(to_assign)
}
