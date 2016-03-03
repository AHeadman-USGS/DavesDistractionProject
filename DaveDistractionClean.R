##Loading necessary libraries

library(dataRetrieval)
library(EGRET)

# functions used in the code
# I currently commented out the "water years" offset and this is running off of calendar years.  
# If you want to change that, be sure and change ALL references to Daily$Day to Daily$WtrDay.
# Adjusting the labels and tabs on the final graphic output is probably a good idea as well.

wtr_yr <- function(dates, start_month) {
  dates.posix = as.POSIXlt(dates)
  #offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  adj.year = dates.posix$year + 1900 #+ offset
  adj.year
}

#Same as wtr_yr, but computes offset based on the day

wtr_day <- function(julianDay) {
  offset = ifelse(julianDay >= 275, -274, 92)
  adj.day = julianDay + offset
  adj.day
}

## Top Five years, Low Five years, currently set to the max values.

HighFive = function(sumtable){
  TopFive = tail(sort(as.vector(sumtable$Max)), 5)
  HighTab = data.frame(WtrYr = integer())
  for (i in TopFive){
    TempTable = sumtable[sumtable[, "Max"] == i,]
    TempYear = max(TempTable$WtrYr, na.rm = TRUE)
    HighTab = rbind(HighTab, data.frame(WtrYr = TempYear, Max = i))
    rm(TempTable)
  }
  HighTab$WtrYr
}

LowFive = function(sumtable){
  LowFive = head(sort(as.vector(sumtable$Max)), 5)
  LowTab = data.frame(WtrYr = integer())
  for (i in LowFive){
    TempTable = sumtable[sumtable[, "Max"] == i,]
    TempYear = max(TempTable$WtrYr, na.rm = TRUE)
    LowTab = rbind(LowTab, data.frame(WtrYr = TempYear, Max = i))
    rm(TempTable)
  }
  LowTab$WtrYr
}

## A couple of variables for presets

ColoradoComp = as.vector(1904:1922)
recentyears = as.vector(1936:2014)

# Section that actually does things.
# Daily variable is the thing to change.

Daily = readNWISDaily("09315000",parameterCd ='00060')
Daily = Daily[ which(Daily$Day != 60), ]
dates=Daily$Date
JulianDay = Daily$Day
WaterYear = wtr_yr(dates,-1)
WaterDay = wtr_day(JulianDay)
Daily$WtrDay = WaterDay
Daily$WtrYr = WaterYear

## Provides a summary data.frame with Mean/Min/Max values

WaterYearSum = data.frame(WtrYr = integer(), Mean = integer(), Min = integer(), Max = integer())
WaterYears = unique(Daily$WtrYr)
for (j in WaterYears){
  tempsub = Daily[Daily[, "WtrYr"] == j,]
  WtrAvg = round(mean(tempsub$Q7), 3)
  WtrMax = round(max(tempsub$Q7), 3)
  WtrMin = round(min(tempsub$Q7), 3)
  WaterYearSum[nrow(WaterYearSum)+1,] = c(j, WtrAvg, WtrMin, WtrMax)
  rm(tempsub)
}

## Removes the last year in the dataset.  For historical gages this is not necessary. But for continuous gages, it is since the
## most recent data occasionally comes with weirdness.

WaterYearSum = head(WaterYearSum, -1)

# Plots a summary of the annual data in a time series over the entire lifetime of the gage.

#bigplot = ggplot(data = WaterYearSum) + geom_line(aes(WtrYr, Mean, colour = "Mean")) + geom_line(aes(WtrYr, Max, colour = "Max")) + geom_line(aes(WtrYr, Min, colour = "Min")) + labs(x="Year",y="Q")+ scale_colour_manual(values= c("Blue", "Green", "Red"))+theme(legend.title=element_blank())
#bigplot

## Using the HighFive/LowFive functions to get a vector from the functions.
HighYrs = HighFive(WaterYearSum)
LowYrs = LowFive(WaterYearSum)

## Creates a vector of the unique years which can be iterated through
## Removes the first year and last year from series for better smoothing action. Not too necessary if wtryr/wtrday adjustments
## are used.

WaterYearsVect = head(unique(WaterYears), -1)
WaterYearsVect = tail(WaterYearsVect, -1)

## Creates the data.frame object for ggplot.
## It currently doesn't line up the day properly if NA values are involved.
## Since this doesn't appear to cause problems with output, I'm leaving it in.
## In a gage with significant/irregular data gaps, this is probably priority 1 on list of things to fix.

x = sort(unique(Daily$Day))
df = data.frame(x)

for (i in WaterYearsVect){
  YVals = Daily[ which(Daily$WtrYr == i), ]$Q7
  pltName = paste( 'Yr', i, sep = '' )
  if (length(YVals) <365){
    YVals = append(YVals, rep(NA, length.out=365-length(YVals)))
    df[pltName] = YVals
  }else{
    df[pltName] = YVals
  }
}

## Modification for making this interactive can happen from here on down.  It's not currently set to do anything too special.


# MatchIhigh > 0 gets blue, MatchILow > 0 gets red, all others get grey
library(gsplot)
library(dinosvg)
gs <- gsplot() %>% title("Green River at Green River, UT (09315000) \n1895-2014")
WaterYearsVectSHORT <- WaterYearsVect#[1:30] # for debugging
# to do: filter out NAs before creating lines
for (i in WaterYearsVectSHORT){
  pltName = paste( 'Yr', i, sep = '' )
  if (match(i, HighYrs, nomatch = 0) > 0){ 
    gs = lines(gs, df[,1], df[[pltName]], col = "blue", id = pltName, class='hidden')
  } else if (match(i, LowYrs, nomatch = 0) > 0){
    gs = lines(gs, df[,1], df[[pltName]],col = "red", id = pltName, class='hidden')
  } else{
    gs = lines(gs, df[,1], df[[pltName]],col = "grey20", opacity='0.8', id = pltName, class='hidden')
  }
}
ylim <- ylim(gs)$side.2
xlim <- xlim(gs)$side.1
gs <- axis(gs, side=1, at = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
           labels=c('Jan','Feb','Mar','Apr','May','June','Jul','Aug','Sep','Oct','Nov','Dec')) %>% 
  text(x=xlim[1],y=ylim[2],labels=" ", id='legend-text')


# note: hand editing the ecmascript, and handediting the legend element
ecma.text = sprintf("function loopYears(){
      clearInterval(window.myInterval);
	  	  	  	var years = [%s];
	  	  var numYears = years.length;
	  	  function displayYear(year){
		  if (document.getElementById('Yr'+year).parentNode.getAttribute('stroke') ===  'rgb(51,51,51)'){
                    document.getElementById('Yr'+year).setAttribute('class','shown');
                    } else {
                    document.getElementById('Yr'+year).setAttribute('class','em-shown');
                    }
                    }
	  	  function legendText(text){
	  	  document.getElementById('legend-text').firstChild.data = text;
	  	  }
	  	  function hideYear(year){
	  	  document.getElementById('Yr'+year).setAttribute('class','hidden');
	  	  }
		  function ghostYear(year){
		  if (document.getElementById('Yr'+year).parentNode.getAttribute('stroke') ===  'rgb(51,51,51)'){
                    document.getElementById('Yr'+year).setAttribute('class','ghost');
                    } else {
                    document.getElementById('Yr'+year).setAttribute('class','em-ghost');
                    }
                    }
	  	  legendText(' ');
	  	  for (var i = 0; i < numYears; i++) {
	  	  hideYear(years[i]);
	  	  }
	  	  var i =0;
	  	  window.myInterval = setInterval(function () {   
	  	  if (i < numYears){
	  	  displayYear(years[i]);
	  	  if (i > 0){
		  ghostYear(years[i-1])
	  	  
	  	  }
	  	  legendText('Year: ' + years[i]);
	  	  i++
	  	  } else {
		  ghostYear(years[i-1])
	  	  clearInterval(window.myInterval);
	  	  }}, 100)
		  
	  	  }",paste(WaterYearsVectSHORT,collapse=','))

style.text = ".shown, .hidden {
    	-webkit-transition: opacity 0.2s ease-in-out;
    	-moz-transition: opacity 0.2s ease-in-out;
    	-o-transition: opacity 0.2s ease-in-out;
    	transition: opacity 0.2s ease-in-out;
    }
  .hidden {
    	opacity:0;
  }
  .em-shown {
		stroke-width: 2;
  }
  .ghost {
  		opacity:0.05;
    	-webkit-transition:all 0.5s ease-in-out;
    	-moz-transition: all 0.5s ease-in-out;
    	-o-transition: all 0.5s ease-in-out;
    	transition: all 0.5s ease-in-out;
  }
  .em-ghost {
  		opacity:0.5;
    	-webkit-transition: all 1.5s ease-in-out;
    	-moz-transition: all 1.5s ease-in-out;
    	-o-transition: all 1.5s ease-in-out;
    	transition: all 1.5s ease-in-out;
		stroke-width: 1;
  }
 text {
 	font-size: 0.8em;
 	cursor: default;
 	font-family: Roboto, Gotham, 'Helvetica Neue', Helvetica, Arial, sans-serif;
 }"
#change to cubic feet per second
#rm grey background, add titles
gs$view.1.2$window$ylab = "Cubic Feet per Second"
gs$view.1.2$window$xlab = "Month"
gs$ecmascript <- ecma.text
gs$css <- style.text
svg(gs, file = "Rplot.svg")

# now read in the svg and manipulate it 
library(XML)
svg <- xmlParse("Rplot.svg", useInternalNode=TRUE)
legend.g <- xpathApply(svg, sprintf("//*[local-name()='g'][@id='%s']","legend-text"))[[1]]
removeAttributes(legend.g, .attrs = 'id')
addAttributes(xpathApply(legend.g,'child::node()')[[1]], .attrs = c('id'="legend-text",dy="1.0em", dx="0.5em", 'text-anchor'='begin' ))#?
newXMLNode(name = 'rect', parent = xpathApply(legend.g,'parent::node()')[[1]],
           attrs=c(x="360", y="70", height="20", width="30", fill="#abccab", stroke="#abccab", 'fill-opacity'="0.4", onclick="loopYears()"))
newXMLNode(name = 'path', parent = xpathApply(legend.g,'parent::node()')[[1]],
           attrs=c(d="M 370,74 L383,80 L370,86z ", fill="#abccab", stroke="none", onclick="loopYears()"))
addAttributes(xpathApply(svg, sprintf("//*[local-name()='g'][@id='%s']/child::node()","axis-label"))[[1]], .attrs = c(dy="-3.0em"))
saveXML(svg, file = "Rplot.svg")

