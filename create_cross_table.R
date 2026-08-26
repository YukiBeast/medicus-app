create_cross_table <- function(item1, item2, data) {
  
  dt1 <- extract(item1, data)
  dt2 <- extract(item2, data)
  
  ct <- dcast(dt1[dt2, on = "id"],
        answer ~ i.answer, )
  
  return(ct)
}

