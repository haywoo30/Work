---
title: "Day15"
author: "Jacob Haywood"
date: "2/26/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
mySum <- function(x=c(1,2,3,4,5)){
  sum <- sum(x)
  return (sum)
}
mySum()
mySum(x=1:10)
mySum(x=c(4,3,-1,17))

sumCumsum <- function(x=c(1,2,3,4,5)){
  s <- sum(x)
  cs <- ave(x,FUN=cumsum)
  newlist <- list(s,cs)
  return(newlist)
}
sumCumsum(c(1,2,3,4,5))
sumCumsum(x=1:10)
sumCumsum(x=c(4,3,-1,17))
```

