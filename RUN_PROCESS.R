# paths
scripts = paste0("scripts/",list.files("scripts"))

# source scripts
lapply(scripts, source)
