
package.skeleton(
	name = "SSMdata",
	environment = .GlobalEnv,
	path = "C:/PART_4/MS/R_packages/", 
	force = FALSE, code_files = character(), encoding = "unknown"
)

library(devtools)

install("C:/PART_4/MS/R_packages/SSM", dependencies = FALSE)

load_all("C:/PART_4/MS/R_packages/SSM")

document(pkg = "C:/PART_4/MS/R_packages/SSM", roclets = NULL, quiet = FALSE)





