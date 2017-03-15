#' A function to create stacked density plot using ggplot
#'
#'
#' @param data Data frame containing columns for density calculation.
#' @param densityColNames The column names of those passed to density calculation.
#' @param stackFactor A factor to create the stacks, length equalt to number of rows in data.
#' @param stackColor Default use rainbow colour palette, but can be specified manually.
#' @param kernel kernel applied for density calculation, includes "gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine".
#' @param bw The smoothing bandwidth to be used. The default, "nrd0", has remained the default for historical and compatibility reasons, rather than as a general recommendation, where e.g., "SJ" would rather fit.
#' @param adjust The bandwidth used is actually adjust*bw.
#' @param reomoveOutliers If coerce the outliers to 1.5*IQR level.
#' @param stackRotation How many degrees to rotate the density stacks.
#' @param stackSeperation Adjust the stack height intervels, use "auto" for automatic adjustment; or specify a value form 0 to 1 times the median height of to all density plots.
#' @param x_text_size Adjust the text size on x ticks.
#' @param strip_text_size Adjust the text size on strip.
#' @param legend_text_size Adjust the text size on legend.
#' @param legendRow The number of rows of the legend.
#' @param legend_title The title of the legend.
#' @param legend_title_size The size of the legend title.
#' @param legend_key_size Adjust the size of the legend key.
#'
#' @import ggplot2
#' @importFrom plyr ldply
#' @importFrom reshape2 melt
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' data <- iris[ ,1:4]
#' densityColNames <- colnames(data)
#' stackFactor <- iris[ ,5]
#' stackedDenistyPlot(data, densityColNames, stackFactor, strip_text_size = 9, legend_text_size = 0.8, legend_key_size = 0.15)
stackedDenistyPlot <- function(data, densityColNames, stackFactor,
                               stackColor,
                               kernel = c("gaussian", "epanechnikov", "rectangular",
                                          "triangular", "biweight",
                                          "cosine", "optcosine"),
                               bw = "nrd0", adjust = 1,
                               reomoveOutliers = FALSE,
                               stackRotation = 0,
                               stackSeperation = "auto",
                               x_text_size = 2,
                               strip_text_size = 7,
                               legend_text_size = 0.5,
                               legendRow = 1,
                               legend_title = "stackName",
                               legend_title_size = 1,
                               legend_key_size = 0.2){


    if(missing(stackFactor)){
        warning("No stackFactor was provided!")
        stackFactor <- rep("stack", length = nrow(data))
    }else if(length(stackFactor) != nrow(data)){
        stop("Length of stackFactor unequal row number of input data!")
    }

    if(missing(densityColNames)){
        densityColNames <- colnames(data)
    }else if(any(!(densityColNames %in% colnames(data)))){
        stop("Unmatch densityColNames found:", paste(densityColNames[!(densityColNames %in% colnames(data))], collapse = " "))
    }

    kernel <- match.arg(kernel)

    if(!is.numeric(stackRotation)){
        stop("stackRotation must be a numeric number")
    }else if(stackRotation < 0 || stackRotation > 90){
        stop("stackRotation must be a numeric number in range 0-90")
    }

    stackCount <- length(unique(stackFactor))
    densityCount <- length(densityColNames)

    ## set the color of stacks
    if(missing(stackColor)){
        stackColor <- rainbow(stackCount)
    }else if(length(stackColor) != stackCount){
        stop("Color provided for stacks doesn't match the number of stacks,
             please check your stackColor parameter!")
    }


    data <- data.frame(data[ ,densityColNames, drop=FALSE],
                       stackFactor = stackFactor,
                       check.names = FALSE)

    densData <- densityCal(data, kernel = kernel, bw = bw,
                            adjust = adjust, reomoveOutliers = reomoveOutliers)

    ## dataframe densData contains {stackName, x , y , densityName}
    xStat <- aggregate(x ~ stackName + densityName, densData, max)
    yStat <- aggregate(y ~ stackName + densityName, densData, max)

    if(stackSeperation == "auto"){
        stackIntervals <- aggregate(y ~ densityName, yStat, function(x){0.8*median(x) * (1-(stackRotation/90)^0.2)^2})
    }else if(stackSeperation < 0 || stackSeperation > 1){
        stop("stackSeperation must be value in range 0-1")
    }else{
        stackIntervals <- aggregate(y ~ densityName, yStat, function(x){median(x)*stackSeperation})
    }

    stackShifts <- aggregate(x ~ densityName, xStat, function(x){max(x) * (stackRotation/90)})

    densData$stack_x <- densData$x + (as.numeric(densData$stackName)-1) * stackShifts$x[match(densData$densityName, stackShifts$densityName)]
    densData$stack_y <- densData$y + (as.numeric(densData$stackName)-1) * stackIntervals$y[match(densData$densityName, stackIntervals$densityName)]

    ## segment lines, x tick, x label
    alignSegments <- ldply(split(densData$x, densData$densityName),
                           function(x){seq(min(x), max(x), length.out=5)},
                           .id = "densityName")
    alignSegments <- melt(alignSegments, id.vars="densityName", variable.name="x_tick", value.name = "x")
    alignSegments$y <- min(densData$y)
    alignSegments$xend <- alignSegments$x + (length(unique(densData$stackName))-1) * stackShifts$x[match(alignSegments$densityName, stackShifts$densityName)]
    alignSegments$yend <- min(densData$y) + (length(unique(densData$stackName))-1) * stackIntervals$y[match(alignSegments$densityName, stackIntervals$densityName)]

    densityHeights <- aggregate(y ~ densityName, yStat, max)
    alignSegments$tickXend <- alignSegments$x
    alignSegments$tickYend <- alignSegments$y - densityHeights$y[match(alignSegments$densityName, densityHeights$densityName)] * 0.01

    ## Automatic adjust the tick text on x axis, default 5 ticks
    tickText <- alignSegments$x
    if(mean(tickText) < 0.0001 || mean(tickText) > 10000){
        tickText <- format(tickText, scientific=TRUE, digits=3)
    }else if(mean(tickText) > 0.1){
        tickText <- round(tickText, digits = 2)
    }
    alignSegments$tickText <- tickText
    alignSegments$textY <- alignSegments$y - densityHeights$y[match(alignSegments$densityName, densityHeights$densityName)] * 0.03


    cat(" Plotting ...\n")
    stackDensityPlot_theme <- theme(legend.position = "top",
                                    legend.title = element_text(size = rel(legend_title_size)),
                                    legend.text = element_text(size = rel(legend_text_size)),
                                    legend.key.size =  unit(legend_key_size, "in"),
                                    strip.text = element_text(size=strip_text_size, lineheight=1, hjust = 0.5, vjust = 0.5),
                                    axis.text.x = element_blank(),
                                    axis.ticks.x = element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    strip.background=element_rect(fill = "grey90", colour = NA))

    gp <- ggplot(densData, aes(x=stack_x, y=stack_y)) +
        geom_segment(data = alignSegments,
                     aes(x = x, y = y, xend = xend, yend = yend),
                     color = "grey80", size=0.3) +
        geom_segment(data = alignSegments,
                     aes(x = x, y = y, xend = tickXend, yend = tickYend),
                     color = "grey20", size=0.3) +
        geom_text(data = alignSegments, aes(x = x, y = textY, label = tickText),
                  hjust = 0.3, vjust = 1.1, size = x_text_size) +
        geom_polygon(aes(fill=stackName, color=stackName), alpha = 0.15) +
        facet_wrap(~densityName, scale = "free") +
        xlab("") + ylab("") +
        scale_fill_manual(values=stackColor) + scale_colour_manual(values=stackColor) +
        guides(col = guide_legend(title = legend_title, nrow = legendRow, byrow = TRUE),
               fill = guide_legend(title = legend_title, nrow = legendRow, byrow = TRUE)) +
        theme_bw() + stackDensityPlot_theme

    return(gp)
}



#' Internal density calculation function serves for \code{stackDenistyPlot}
#'
#'
#' @param data Input data frame including variable columns and stackFactor column.
#' @param kernel Kernel applied for density calculation, includes "gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine".
#' @param bw The smoothing bandwidth to be used. The default, "nrd0", has remained the default for historical and compatibility reasons, rather than as a general recommendation, where e.g., "SJ" would rather fit.
#' @param adjust The bandwidth used is actually adjust*bw.
#' @param reomoveOutliers If coerce the outliers to 1.5*IQR level.
#'
#' @return data frame containing stackName, x, y and densityName
#' @importFrom plyr ldply
#'
#' @examples
#' data <- iris
#' data$stackFactor <- data$Species
#' data$Species <- NULL
#' dr <- densityCal(data = data)
densityCal <- function(data, kernel = c("gaussian", "epanechnikov", "rectangular",
                                         "triangular", "biweight", "cosine", "optcosine"),
                        bw = "nrd0", adjust = 1, reomoveOutliers = FALSE){
    cat("  Calculating Density for each stack column...\n")
    kernel <- match.arg(kernel)
    #print(table(data$stackFactor))
    dataBystackFactor <- split(subset(data, select = -stackFactor), data$stackFactor)
    densityWrap <- function(d, ...){
        resOut <- NULL
        for(i in colnames(d)){
            x <- d[,i]
            if(reomoveOutliers){
                cat("  Remove outliers...\n")
                x_IQR <- IQR(x)
                x_lowLimit <- quantile(x, 0.25) - 1.5 * x_IQR
                x_highLimit <- quantile(x, 0.75) + 1.5 * x_IQR
                x <- x[x >= x_lowLimit && x <= x_highLimit]
            }
            dens <- density(x, ...)
            densOut <- data.frame(x=dens$x, y=dens$y, densityName = i)
            resOut <- rbind(resOut, densOut)
        }
        return(resOut)
    }

    r <- ldply(dataBystackFactor, densityWrap,
               kernel = kernel, bw = bw, adjust = adjust,
               .progress = "text",
               .id = "stackName")
    return(r)
}




