returnPalette <-
    function(ColorVariable = NULL,
             GNE.colors = F,
             jumble = T) {
        # #<---------------------------->
        # # You must include this section when:
        # # Distributing, Using and/or Modifying this code.
        # # Please read and abide by the terms of the included LICENSE.
        # # Copyright 2023, Deepankar Chakroborty, All rights reserved.
        # #
        # #  Author : Deepankar Chakroborty (https://github.com/dchakro)
        # #  Website: https://www.dchakro.com
        # #  Report issues: https://github.com/dchakro/shared_Rscripts/issues
        # #  License: https://github.com/dchakro/shared_Rscripts/blob/master/LICENSE
        # #<---------------------------->
        
        
        # #  PURPOSE:
        # #  Generates an appropriate palette for categorical data based on its length
        
        
        if (class(ColorVariable) == "character") {
            colorsNeeded <- length(unique(ColorVariable))
        }
        if (class(ColorVariable) == "factor") {
            colorsNeeded <- length(levels(ColorVariable))
        }
        if (colorsNeeded < 1) {
            myPalette <- c("#DD0000")
            warning("Nothing to return color for.")
        }
        if (colorsNeeded > 1 &
            colorsNeeded < 10 & GNE.colors == T) {
            myPalette <-
                c(
                    "#003087",
                    "#C8102E",
                    "#008C15",
                    "#FF8200",
                    "#AF1685",
                    "#6F3B34",
                    "#29D991",
                    "#cd970b",
                    "#8F2257",
                    "#00A3E0"
                )[1:colorsNeeded]
        } else if (colorsNeeded > 1 & colorsNeeded < 10) {
            myPalette <- palette.colors(n = colorsNeeded,
                                        palette = "Classic Tableau")
        } else if (colorsNeeded >= 10 & colorsNeeded < 18) {
            set.seed(2023)
            myPalette <- c(
                palette.colors(n = 10,
                               palette = "Classic Tableau"),
                palette.colors(n = colorsNeeded - 10,
                               palette = "Set3")
            )
            set.seed(2023)
            myPalette <- sample(myPalette)
        } else if (colorsNeeded >= 18 & jumble) {
            myPalette <- sample(hcl.colors(n = colorsNeeded,
                                           palette = "Spectral"))
        } else if (colorsNeeded >= 18 & !jumble) {
            myPalette <- hcl.colors(n = colorsNeeded,
                                    palette = "Spectral")
        }
        return(myPalette)
    }