# Originally based on code from Fernando Henrique Ferraz Pereira da Rosa 
# http://www.fernandohrosa.com.br/en/P/shaded_areas_in_r/

# The parameters that don't change much.
Resolution <- 0.01
BotYBound <- 0

# Uncomment just one of the following lines to select function!

mainLabel <- "Standard Normal Density, crit = 2.4"; fn <- dnorm; xMaxFn = 0; leftXBound <- -3.5; rightXBound <- 3.5; critValue <- -2.1
#mainLabel <- "t distribution, df = 1"; fnA <- dt; fn <- function (x) { return (fnA (x, 1)) }; xMaxFn = 0; leftXBound <- -5; rightXBound <- 5; critValue <- -2.7

#mainLabel <- "chi-Sq distribution, df = 1, crit = 2.6"; fnA <- dchisq; fn <- function (x) { return (fnA (x, 1)) }; xMaxFn = 25; critValue <- 2.6; leftXBound <- 0; rightXBound <- 3; topYBound <- 4.2
#mainLabel <- "chi-Sq distribution, df = 2, crit = 7"; fnA <- dchisq; fn <- function (x) { return (fnA (x, 2)) }; xMaxFn = 25; critValue <- 7; leftXBound <- 0; rightXBound <- 8; topYBound <- 0.55
#mainLabel <- "chi-Sq distribution, df = 3, crit = 8.2"; fnA <- dchisq; fn <- function (x) { return (fnA (x, 3)) }; xMaxFn = 5; leftXBound <- 0; rightXBound <- 10; critValue <- 8.2; topYBound <- 0.25
#mainLabel <- "chi-Sq distribution, df = 6, crit = 13"; fnA <- dchisq; fn <- function (x) { return (fnA (x, 6)) }; xMaxFn = 13; critValue <- 13; leftXBound <- 0; rightXBound <- 15; topYBound <-0.15
#mainLabel <- "chi-Sq distribution, df = 8, crit = 17"; fnA <- dchisq; fn <- function (x) { return (fnA (x, 8)) }; xMaxFn = 25; critValue <- 17; leftXBound <- 0; rightXBound <- 20; topYBound <-0.12
#mainLabel <- "chi-Sq distribution, df = 10, crit = 25"; fnA <- dchisq; fn <- function (x) { return (fnA (x, 10)) }; xMaxFn = 25; critValue <- 22; leftXBound <- 0; rightXBound <- 25; topYBound <-0.11

# Note to self, do NOT title a variable df again!!
#mainLabel <- "F distribution, df1 = 3, df2 = 5, crit = 21.3"; fnA <- df; fn <- function (x) { return (fnA (x, 3, 5)) }; xMaxFn = .3; leftXBound <- 0; rightXBound <- 6; critValue <- 5.2
#mainLabel <- "F distribution, df1 = 6, df2 = 8, crit = 4.2"; fnA <- df; fn <- function (x) { return (fnA (x, 6, 8)) }; xMaxFn = .5; leftXBound <- 0; rightXBound <- 5; critValue <- 4.2

# Can use these to override the function-specific settings.
#leftXBound <- -2.8
#rightXBound <- 2.8
#critValue <- 25

# This is not the greatest, but as long as you define xMaxFn in the above 
# lines, it'll make sure it doesn't clip.
topYBound <- fn (xMaxFn) * 1.1
#topYBound <- 0.075

# Create the distribution curve, no critical value or shading yet.
xSamples = (rightXBound - leftXBound)/Resolution
curve ( fn (x),
        xlim = c (leftXBound, rightXBound), 
        ylim = c (BotYBound, topYBound),
        n = xSamples,
        main = mainLabel )

# What we're shading is pretty close to a trapezoid with 4 vertices, the top 
# part will be a sequence of points instead of a straight line. For both left 
# and right-tailed tests, will start from lower-left then upper left, upper 
# right, then lower right. Will always call the fn to get the y-values, so its
# only the x-values that are different.
if (critValue < 0)
{
   # For left-tailed test, left side is the left boundary
   x1 <- leftXBound
   x2 <- critValue
} else if (critValue > 0)
{
   # For right-tailed test, left side is the critical value
   x1 <- critValue
   x2 <- rightXBound
} else # critValue = 0
{
   x1 <- critValue
   x2 <- critValue
}
y1 <- BotYBound

# Create a polygon for the shaded area with lists of point coordinates. First
# vertex is lower left corner: (x1,0).
coord.x <- c (x1)
coord.y <- c (y1)

# Keep extending the list for each point. Draw the curved top from 
# upper left to upper right with a sequence of points from (-3,f(-3)) to 
# (critValue,(f(critValue))).
coord.x <- c ( coord.x,
               seq (x1, x2, Resolution) )
coord.y <- c ( coord.y,
               fn (seq (x1, x2, Resolution)) ) 

# Last vertex: (x2, y1)
coord.x <- c (coord.x, x2)
coord.y <- c (coord.y, y1)

# List of points is done. Draw the polygon!
#polygon (coord.x, coord.y, col='lightgreen')

# Close off the bottom x-axis with a horizontal line.
#abline(h=0)
