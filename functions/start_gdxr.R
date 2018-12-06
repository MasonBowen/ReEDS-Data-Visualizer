# This code is essentially to read in all non-gdx related info such as plotting colors, technology types and other such things

# Copying heavily from Yinong's code

library(gdxrrw)

gms.dir <- gams.directory

GAMSVersions<-c("24.4","24.7","24.6","24.5","24.3","24.2","24.1", "25.1")
for (version in GAMSVersions){
  # FOLLOWING ASSUMES GAMS FILE LOCATED in program files (x86), may be elsewhere
  if (dir.exists(file.path(gms.dir, version))){
    Selected_GAMSVersion<-version
    break
  }
}
gams.folder <- paste0("C:/Program Files (x86)/GAMS/win64/",Selected_GAMSVersion,"/")



start_gams <- function(dir = paste0("/../Program Files (x86)/GAMS/win64/",Selected_GAMSVersion)) {
  # Try to load package
  out <- require(gdxrrw)
  
  if (!out) {
    print("Error: gdxrrw package not installed")
    print("  Go to ReEDS-R Readme file for installation instructions")
  } else {
    out2 <- igdx(dir)
    if(!out2) {
      print("Error: gdxrrw package not properly loaded")
      print("  Use start_gams(dir), where 'dir' is your GAMS installation directory")
    }
  }
}

start_gams()

