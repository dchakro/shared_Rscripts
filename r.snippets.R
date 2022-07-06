snippet comment_date
	`r paste("#", date(), "------------------------------\n")`

snippet mat
	matrix(${1:data}, nrow = ${2:rows}, ncol = ${3:cols})

snippet elif
	if (${1:condition}) {
		${0}
	}	else if (${1:condition}) {
		${0}
	}	else {
		${0}
	}

snippet fun
	${1:name} <- function(${2:variables}) {
		## Description:		
		## Input:
		## Output:
		
		${0}
	}

snippet gsubstitute
	gsub("${1:Find}","${2:Replace}",${3:variable}, fixed = T)

snippet replaceText
	regmatches(${1:String}, gregexpr(pattern="${2:Find}", text =${1:String}, fixed = T) <- "${3:newText}"

snippet not%
	'%nin%' <- Negate('%in%')

snippet split
	data.table::as.data.table(stringi::stri_split_fixed(str = ${1:String}, pattern = "${2:pattern}", simplify = T))

snippet find_matches
	regmatches(${1:String}, gregexpr(pattern="${2:Find}",text =${1:String}, fixed = T)

snippet install
	to_install <- c("${1:pack1}")	## Set packages to install here and run
	if (unname(Sys.info()["sysname"]) == "Darwin") {
		CPU_cores <-
		as.integer(system(command = "sysctl -n hw.physicalcpu", intern = T))
		# logical cores
		# CPU_cores <- system("sysctl -n hw.ncpu")
	} else if (unname(Sys.info()["sysname"]) == "Linux") {
		CPU_cores <- as.integer(system(command = "nproc", intern = T))
	}
	options(Ncpus = CPU_cores)
	utils::setRepositories(ind = c(1, 2, 3))
	missing_packages <-
		to_install[!(to_install %in% installed.packages()[, "Package"])]
	if (length(missing_packages))
		install.packages(missing_packages, Ncpus = getOption("Ncpus", 1L))
	rm(missing_packages, to_install, CPU_cores)

snippet roundUp 
	source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/roundUp.R")
	roundUp(x = ${1:number}, to = ${2:10})

snippet ggtheme
	library(ggplot2)
	source('https://raw.githubusercontent.com/dchakro/ggplot_themes/master/DC_theme_generator.R')
	customtheme <- DC_theme_generator(type = "L")


snippet ggarea
	ggplot(data=${1:longDF}, aes(x=${2:orderedVar2}, y=${3:plottingValue})) + 
		geom_area(alpha=1, color="black", aes(fill=${4:fill.order}), position=position_fill(reverse = T)) + 
		ylab("Percentage of total")

snippet percentScale
	scale_y_continuous(breaks = seq(0, 1, by=0.25),labels = paste((seq(0, 1, by = .25)*100), "%", sep=""))
	
snippet bench
	bmark <- microbenchmark(
	"${1:method1}" = {
	  	# Describe actions
	 }
	 },
	"${2:method2}" = {
	  # Describe actions
	  }
	}, times = 5 )
	source("https://raw.githubusercontent.com/dchakro/shared_Rscripts/master/summarySE.R")
	DF <- summarySE(bmark,measurevar = "time",groupvars = "expr",statistic = "mean")
