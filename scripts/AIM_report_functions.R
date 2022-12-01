# Functions for the AIM ANNUAL COMPLIANCE REPORTS LIVE HERE


USDA_point_estimator <- function(x, CatCol, GrpCol, LookUpTab, LUTCol){
  
  # This functions serves to take in categorically textured soils data
  # e.g. ('sandy loam') and generate random values within that class
  # corresponding to the three components of soil (silt, sand, clay), 
  # for creating PLOTS for visual display of information. THIS METHOD
  # CANNOT AND IS NOT INTENDED TO retrieve true values from categorical data
  # which is impossible. 
  
  # Inputs: 'x' = a dataframe containing at least a column of unambigious 
  # texture classes, 'CatCol' = the column containing the categorical texture class
  # information. 'GrpCol' = a column(s) containing information on grouping which 
  # will be relevant for plotting downstream (note these columns not used 
  # in this function 'LookUpTab' = a 4 column table containing the extremities 
  # of the 3 soil components (see USDA example from ggtern). 'LUTCol' 
  # the column containing the name the categorical soil types
  
  # Side effects a NEW DATAFRAME for plotting with ggtern is created
  
  CatCol <- enquo(CatCol)
  GrpCol <- enquo(GrpCol)
  LUTCol <- enquo(LUTCol)
  
  # NOTE TO SAGESTEPPE : NEED TO IMPLEMENT SOME ... TO DEAL WITH MULTIPLE
  # LEVELS OF GROUPING 
  # https://tidyeval.tidyverse.org/multiple.html
  
  dataset <- x %>% 
    select(!!CatCol, !!GrpCol) %>% 
    mutate(CatCol_FN = case_when(
      Texture == 'C' ~ 'Clay' ,
      Texture == 'CL' ~ 'Clay Loam' ,
      Texture == 'L' ~ 'Loam' ,
      Texture == 'LS' ~ 'Loamy Sand' ,
      Texture == 'S' ~ 'Sand' ,
      Texture == 'SC' ~ 'Sandy Clay',
      Texture == 'SCL' ~ 'Sandy Clay Loam' ,
      Texture == 'SI' ~ 'Silt',
      Texture == 'SIC' ~ 'Silty Clay' ,
      Texture == 'SICL' ~ 'Silty Clay Loam',
      Texture == 'SIL' ~ 'Silty Loam' ,
      Texture == 'SL' ~ 'Sandy Loam' 
      )
    ) 
  
  LookUpTab1 <- LookUpTab %>% 
    group_by(!!LUTCol) %>% 
    summarise(across(where(is.numeric), # Get stats for these columns 
                  funs(min = min,
                       max = max),
                  .names = "{.col}.{.fn}"
    ))
  
  return(LookUpTab1)
  
}

out <- USDA_point_estimator(x = soildata, CatCol = Texture, GrpCol = Actual.Eco.Site, LookUpTab = USDA, LUTCol = Label)


