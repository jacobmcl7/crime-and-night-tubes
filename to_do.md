to do list:

next things to do:
 - think about how to demonstrate substitution - use different control groups, and plot means? 
   - use regions within 1km of other stations, and separately use regions just between 1 and 2km of treated stations, then just look at whether the control regions go down or not
 - get all notes into one place - move from overleaf
 - get in the appropriate covariates - e.g. region and station properties
    - while doing this, maybe undo the splitting of files for the geocoding - we don't need to do this
    - controls will include: distance to 1st/2nd/3rd nearest station (collinear with treatment? maybe not, if control stations used), nearest station FEs, number of stations within 2km, population density, nearby income/IMD, distance to central London(?), land use mix, property values, local employment rates, other stuff. All interacted with time!
 - check the new data construction is done properly
 - sort out BJS estimation
 - organise the data cleaning much better! merge the parallel cleaning processes together, for example
 - organise the data analysis much better! maybe define treatment in the cleaning, for example, and make the functions nicer
 - get SEs for the nonparametric estimation
 - Poisson graphing wrong? check intercept at et = -1