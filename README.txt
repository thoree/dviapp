### Deploy a new version
library(rsconnect)
rsconnect::deployApp('C:/Users/theg/Dropbox/Rlibs/dviapp')
### Need version 2.2.0 of dvir:
remove.packages("dvir")
install.packages("remotes")
library(remotes)
install_version("dvir", "2.2.0")