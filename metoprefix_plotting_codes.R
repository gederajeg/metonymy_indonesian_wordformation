# Note:
# to save the plot into the current working directory,
# uncomment the line beginning with `png(...)` and `dev.of(...)`.
# Otherwise, the resulting plot will be viewed in RStudio plot panel or in the R plot window.

# load the data
load("gpwrajeg_2013_metoprefix.RData")

# edit the zero prefix to be represented as unicode character
names(num.meto) <- gsub("\xd8", paste("\U00D8"), names(num.meto), perl = TRUE)
dframestrip$prefixes <- gsub("\xd8", paste("\U00D8"), dframestrip$prefixes, perl = TRUE)
prefixes <- gsub("\xd8", paste("\U00D8"), prefixes, perl = TRUE)
sourcetarget$prefix <- gsub("\xd8", paste("\U00D8"), sourcetarget$prefix, perl = TRUE)

# store the default `par()`
par_default <- par(no.readonly = TRUE)

# Figure 1 ---------

# png("specificity of prefixes-histo-tes.png", res = 400, height = 2000, width = 3600) # uncomment this line to output the plot into the current working directory
par(family = "serif")
par(fig = c(0, 0.5, 0, 0.8), las = 1)

# histogram ----------
hist(num.meto, 
     freq = FALSE, 
     breaks = brksm, 
     col = "lightgrey", 
     border = "darkgrey", 
     xlim = c(-10, 30), 
     ylim = c(0, 0.4), 
     xaxt = "n", 
     main = "", 
     xlab = "Number of metonymy patterns", 
     cex.axis = 0.65, 
     cex.lab = 0.9)
axis(side = 1, at = seq(-10, 30, by = 3), cex.axis = 0.65)
x <- num.meto
curve(dnorm(x, mean = mean(num.meto), sd = sd(num.meto)), add = TRUE, lwd = 2)
abline(v = mean(num.meto), lty = 3)
abline(v = median(num.meto), col = "red")
lines(density(num.meto), lty = 2)

par(fig = c(0.5, 1, 0, 0.8), new = TRUE, las = 1)
hist(num.wclass, 
     freq = FALSE, 
     breaks = brkswc, 
     xlim = c(-10, 30), 
     ylim = c(0, 0.4), 
     col = "lightgrey", 
     border = "darkgrey", 
     xaxt = "n", 
     main = "", 
     xlab = "Number of word class patterns", 
     cex.axis = 0.65, 
     cex.lab = 0.9)
axis(side = 1, at = seq(-10, 30, by = 3), cex.axis = 0.65)
x <- num.wclass
curve(dnorm(x, mean = mean(num.wclass), sd = sd(num.wclass)), add = TRUE, lwd = 2)
abline(v = mean(num.wclass), lty = 3)
abline(v = median(num.wclass), col = "red")
lines(density(num.wclass), lty = 2)

# boxplot (superimposed) ------------
par(fig = c(0, 0.5, 0.5, 1), new = TRUE)
boxplot(num.meto, horizontal = TRUE, axes = FALSE, ylim = c(-10, 30), notch = FALSE)
points(mean(num.meto), order(mean(num.meto)), pch = 18) # adding means to horizontal boxplot

par(fig = c(0.5, 1, 0.5, 1), new = TRUE)
boxplot(num.wclass, horizontal = TRUE, axes = FALSE, ylim = c(-10, 30), notch = FALSE)
points(mean(num.wclass), order(mean(num.wclass)), pch = 18) # adding means to horizontal boxplot
mtext("Figure 1. Histogram & boxplot for metonymy & word class patterns", 
      side = 3, 
      outer = TRUE, 
      line = -3, 
      font = 2, 
      cex = 1.25)
rm(x)
# dev.off() # uncomment this line to output the plot into the current working directory

# Figure 2 ---------
# metonymy --------
# png("scattermetopreftext.png", res = 500, width = 4500, height = 2000) # uncomment this line to output the plot into the current working directory
par(mfrow = c(1, 2))
par(family = "serif", las = 1)
plot(NA,
     xlim = c(1, 8),
     ylim = c(0, 25), 
     cex.axis = 0.7, 
     xaxt = "n", 
     bty = "l", 
     xlab = "", 
     ylab = "Number of patterns per prefix", 
     cex.lab = 0.9)

mtext(text = "Plot for metonymy patterns", 
      side = 1, 
      at = 5, 
      line = 1, 
      cex = 0.9)
text(x = seq(1, 8, by = 1), 
     y = dframestrip[1:8, 1], # y-axis represents the number of metonymy patterns per prefix
     labels = dframestrip$prefixes, 
     cex = 0.8, 
     font = 3, 
     family = "serif")
abline(h = mean.meto, lty = 2, col = "red", lwd = 1.5)
lines(c(0:8), rep(sd.1.above.meto, 9), lwd = 1, lty = 1)
lines(c(0:8), rep(sd.1.below.meto, 9), lwd = 1, lty = 1)

# word class --------
par(family = "serif")
plot(NA, 
     xlim = c(1, 8), 
     ylim = c(0, 25), 
     cex.axis = 0.7, 
     xaxt = "n", 
     bty = "l", 
     xlab = "", 
     ylab = "Number of patterns per prefix",
     cex.lab = 0.9)

mtext(text = "Plot for word class patterns", 
      side = 1, 
      at = 5, 
      line = 1, 
      cex = 0.9)
text(x = seq(1, 8, by = 1), 
     y = dframestrip[9:16, 1], # y-axis represents the number of word-class patterns per prefix
     labels = dframestrip$prefixes, 
     cex = 0.8, 
     font = 3, 
     family = "serif")
abline(h = mean.wclass, lty = 2, col = "red", lwd = 1.5)
lines(c(0:8), rep(sd.1.below.wc, 9), lwd = 1, lty = 1)
lines(c(0:8), rep(sd.1.above.wc, 9), lwd = 1, lty = 1)
mtext("Figure 2. Scatter plot for metonymy & word class patterns per prefix", 
      side = 3, 
      outer = TRUE, 
      line = -2, 
      font = 2, 
      cex = 1.225)
# dev.off() # uncomment this line to output the plot into the current working directory

# Figure 3 ------------
# Creating Data Frame to be plotted in Figure 3 -------------
Target <- c("Prefixes with 1 Target", "Prefixes with > 1 Target", "Prefixes with Target > Sources")
Metonymy <- c(25, 75, 25)
WordClass <- c(50, 50, 13)
TargetSpec <- data.frame(Target, Metonymy, WordClass) # Data frame 'TargetSpec'

# set the `par()`
par(par_default)

# Plotting Figure 3 ------------
# png("target specificity.png", res = 300, height = 700, width = 950) # uncomment this line to output the plot into the current working directory
par(mar = c(4, 5, 3, 1), 
    omi = c(0, 0.1, 0, 0.1), 
    mgp = c(3, 0.5, 0), 
    las = 1, 
    mex = 0.5, 
    cex.main = 0.55, 
    cex.lab = 0.5, 
    cex.axis = 0.5, 
    family = "serif")
bar.plot <- barplot(as.matrix(TargetSpecPerc[, 2:3]), 
                    beside = TRUE, 
                    legend.text = TargetSpecPerc$Target, 
                    args.legend = list(bty = "n", 
                                       horiz = FALSE, 
                                       cex = 0.45), 
                    ylim = c(0, 100), 
                    ylab = "Percentage", 
                    main = "Figure 3. Specificity of prefixes in terms of Targets", 
                    cex.axis = 0.5, 
                    cex.names = 0.5, 
                    space = c(0, 3), 
                    border = "white")
bar.labels <- as.matrix(TargetSpecPerc[, 2:3])
text(bar.plot, bar.labels + 5, 
     labels = as.character(bar.labels), 
     cex = 0.5)
# dev.off() # uncomment this line to output the plot into the current working directory