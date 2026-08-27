assign_labels <- function(code, data) {

  labels <- var_label(data)
  
  to_assign <- vapply(code,
                      function(x) gsub("^.*: ", "", labels[[x]]),
                      character(1))
  
  return(to_assign)
}
