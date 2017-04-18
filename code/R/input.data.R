getParam1 <- function(df, key){
  param1 <- df$Param1[df$Variable == key]
  return(param1)
}

getParam2 <- function(df, key){
  param2 <- df$Param2[df$Variable == key]
  return(param2)
}

getDistribution <- function(df, key){
  dist <- df$Distribution[df$Variable == key]
  if(dist == "Normal"){
    return(rnorm)
  }else if (dist == "Bernoulli"){
    return(rbinom)
  }else if(dist == "Gamma"){
    return(rgamma)
  }else{
    stop("Unknown distribution.")
  }
}


generateTheoreticalData <- function(N, class.label, class.file){
  
  probData <- read.csv(class.file) ## probability model to generate data from
  
  ## extract the probabilities 
  probHasAuthor <- getParam1(probData, "hasAuthor")
  probHasOrganization <- getParam1(probData, "hasOrganization")
  probHasTitle <- getParam1(probData, "hasTitle")
  probHasDescription <- getParam1(probData, "hasDescription")
  probUsedTaxonID <- getParam1(probData, "usedTaxonIDNumber")
  probOpenSite <- getParam1(probData, "configdata.state.openSite")
  probWestPanelOpen <- getParam1(probData, "configdata.state.layout.westPanelIsOpen")
  probSouthPanelOpen <- getParam1(probData, "configdata.state.layout.southPanelIsOpen")
  probEastPanelOpen <- getParam1(probData, "configdata.state.layout.eastPanelIsOpen")
  probDoSearch <- getParam1(probData, "configdata.state.doSearch")
  
  ## these are two parameter distributions --> todo: make this better code
  zoom1 <- getParam1(probData, "configdata.state.map.zoom")
  zoom2 <- getParam2(probData, "configdata.state.map.zoom")
  
  totalTime1 <- getParam1(probData, "configdata.config.timer.totalElapsed")
  totalTime2 <- getParam2(probData, "configdata.config.timer.totalElapsed")
  
  dataTime1 <- getParam1(probData, "configdata.config.timer.loadElapsed")
  dataTime2 <- getParam2(probData, "configdata.config.timer.loadElapsed")
  
  
  ## For boolean layout data 
  ## compute probability of 1 success in 1 trial with probability p 
  ## repeat N times for N test 'maps'
  trials <- 1
  model <- list()
  model$hasAuthor <- as.logical(rbinom(N, trials, probHasAuthor))
  model$hasOrganization <- as.logical(rbinom(N, trials, probHasOrganization))
  model$hasTitle <- as.logical(rbinom(N, trials, probHasTitle))
  model$hasDescription <- as.logical(rbinom(N, trials, probHasDescription))
  model$usedTaxonIDNumber <- as.logical(rbinom(N, trials, probUsedTaxonID)) ## more likely to browse than search
  model$configdata.state.openSite <- as.logical(rbinom(N, trials, probOpenSite)) ## not likely to identify specific sites 
  model$configdata.state.layout.westPanelIsOpen <- as.logical(rbinom(N, trials, probWestPanelOpen)) ## not likely to identify specific sites
  model$configdata.state.layout.southPanelIsOpen <- as.logical(rbinom(N, trials, probSouthPanelOpen)) ## not likely to change default, and provides context
  model$configdata.state.layout.eastPanelIsOpen <- as.logical(rbinom(N, trials, probEastPanelOpen)) ## not likely to change default
  model$configdata.state.doSearch <- as.logical(rbinom(N, trials, probDoSearch)) ##student's unlikely to share without data on map
  
  ## for continuous data 
  model$configdata.state.map.zoom <- rgamma(N, zoom1, zoom2)
  ## don't do pitch and bearing 
  ## don't do geographic center

  model$configdata.config.timer.totalElapsed <- rgamma(N, totalTime1, totalTime2)
  
  model$configdata.config.timer.loadElapsed <- rgamma(N, dataTime1, dataTime2)
  
  ## convert to data frame
  model.df <- do.call(cbind, lapply(model, data.frame, stringsAsFactors=FALSE))
  names(model.df) <- names(model)
  
  ## assign class label
  model.df$class <- class.label
  return(model.df)
}



