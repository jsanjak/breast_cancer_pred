#helpers.R contains helper functions and variable definitions

#Function to convert variable titles into english
title_convert <- function(xstr){
  xx <- paste(strsplit(tools::toTitleCase(xstr),"_")[[1]],collapse=" ")
  if (xx == "Radius mean"){
    xx <- "Radius mean (mm)"
  } else if ( xx == "Texture mean") {
    xx <- "Texture mean (gray scale pixels)"
  } else if ( xx == "Perimeter mean") {
    xx <- "Perimeter mean (mm)"
  } else if ( xx == "Area mean") {
    xx <- "Area mean (mm^2)"
  } else if ( xx == "Smoothness mean") {
    xx <- "Smoothness mean (mm)"
  } else if ( xx == "Compactness mean") {
    xx <- "Compactness mean"
  } else if ( xx == "Fractal dimension mean") {
    xx <- "Fractal dimension mean (coastline approx.)"
  } else if (xx == "Radius se"){
    xx <- "Radius se (mm)"
  } else if ( xx == "Texture se") {
    xx <- "Texture se (gray scale pixels)"
  } else if ( xx == "Perimeter se") {
    xx <- "Perimeter se (mm)"
  } else if ( xx == "Area se") {
    xx <- "Area se (mm^2)"
  } else if ( xx == "Smoothness se") {
    xx <- "Smoothness se (mm)"
  } else if ( xx == "Compactness se") {
    xx <- "Compactness se"
  } else if ( xx == "Fractal dimension se") {
    xx <- "Fractal dimension se (coastline approx.)"
  } else if (xx == "Radius worst"){
    xx <- "Radius worst (mm)"
  } else if ( xx == "Texture worst") {
    xx <- "Texture worst (gray scale pixels)"
  } else if ( xx == "Perimeter worst") {
    xx <- "Perimeter worst (mm)"
  } else if ( xx == "Area worst") {
    xx <- "Area worst (mm^2)"
  } else if ( xx == "Smoothness worst") {
    xx <- "Smoothness worst (mm)"
  } else if ( xx == "Compactness worst") {
    xx <- "Compactness worst"
  } else if ( xx == "Fractal dimension worst") {
    xx <- "Fractal dimension worst (coastline approx.)"
  }
  return(xx)
}

#Function to color code true/false positives/negatives
predColor <- function(pred,truth){
  
  output <- rep(0,length(pred))
  for( i in 1:length(pred)){ 
    if(truth[i]==pred[i]){
      if (truth[i]==1){
        output[i] <- "Correctly benign"
      } else {
        output[i] <- "Correctly malignant"
      }
      
    } else {
      if (truth[i]==1){
        output[i] <- "Incorrectly benign"
      } else {
        output[i] <- "Incorrectly malignant"
      }
    }
  }
  return(output)
}

####
# Benign Malignant labels
break_label = function(breaks){
  labs = ifelse(breaks==0,"B","M")
  return(labs)
}

# Color pallette for plotting
cbbPalette <- c("#000000", 
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2", 
                "#D55E00", 
                "#CC79A7")