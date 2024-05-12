setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")

library(ggplot2)
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()
base+geom_smooth()

# To override the data, you must use %+%
base %+% subset(mpg, fl == "p")

# Alternatively, you can add multiple components with a list.
# This can be useful to return from a function.
base + list(subset(mpg, fl == "p"), geom_smooth())

+aes(x = mpg, y = wt)
aes(mpg, wt)
# You can also map aesthetics to functions of variables
aes(x = mpg ^ 2, y = wt / cyl)
# Or to constants
aes(x = 1, colour = "smooth")
# Aesthetic names are automatically standardized
aes(col = x)
aes(fg = x)
aes(color = x)

aes(colour = x)
# aes() is passed to either ggplot() or specific layer. Aesthetics supplied
# to ggplot() are used as defaults for every layer.
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggplot(mpg) + geom_point(aes(displ, hwy))
# Tidy evaluation---------------------------------------------------
# aes() automatically quotes all its arguments, so you need to use tidy
# evaluation to create wrappers around ggplot2 pipelines. The
# simplest case occurs when your wrapper takes dots:
scatter_by <- function(data, ...) {
  ggplot(data) + geom_point(aes(...))
}
scatter_by(mtcars, disp, drat)
# If your wrapper has a more specific interface with named arguments,
# you need the "embrace operator":
scatter_by <- function(data, x, y) {
  ggplot(data) + geom_point(aes({{ x }}, {{ y }}))
}
scatter_by(mtcars, disp, drat)
# Note that users of your wrapper can use their own functions in the
# quoted expressions and all will resolve as it should!
cut3 <- function(x) cut_number(x, 3)
scatter_by(mtcars, cut3(disp), drat)

# Bar chart example
p <- ggplot(mtcars, aes(factor(cyl)))
# Default plotting
p + geom_bar()
# To change the interior coloring use fill aesthetic
p + geom_bar(fill = "red")
# Compare with the color aesthetic which changes just the bar outline
p + geom_bar(colour = "red")
# Combining both, you can see the changes more clearly
p + geom_bar(fill = "white", colour = "red")
# Both color and fill can take an #RRGGBB specification.
p + geom_bar(fill = "#00abff")
# Use NA for a completely transparent color.
p + geom_bar(fill = NA, colour = "#00abff")
# Coloring scales differ depending on whether a discrete or
# continuous variable is being mapped. For example, when mapping
# fill to a factor variable, a discrete color scale is used.
ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
# When mapping fill to continuous variable a continuous color
# scale is used.
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))

# Some geoms only use the color aesthetic but not the fill
# aesthetic (e.g. geom_point() or geom_line()).
p <- ggplot(economics, aes(x = date, y = unemploy))
p + geom_line()
p + geom_line(colour = "green")
p + geom_point()
p + geom_point(colour = "red")
# For large datasets with overplotting the alpha
# aesthetic will make the points more transparent.
set.seed(1)
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
p <- ggplot(df, aes(x,y))
p + geom_point()
p + geom_point(alpha = 0.5)
p + geom_point(alpha = 1/10)
# Alpha can also be used to add shading.
p <- ggplot(economics, aes(x = date, y = unemploy)) + geom_line()
p
yrng <- range(economics$unemploy)
p <- p +
  geom_rect(
    aes(NULL, NULL, xmin = start, xmax = end, fill = party),
    ymin = yrng[1], ymax = yrng[2], data = presidential
  )
p
p + scale_fill_manual(values = alpha(c("blue", "red"), .3))

# These functions can be used inside the `aes()` function
# used as the `mapping` argument in layers, for example:
# geom_density(mapping = aes(y = after_stat(scaled)))
after_stat(x)
after_scale(x)
stage(start = NULL, after_stat = NULL, after_scale = NULL)

# 'x' and 'y' are mapped directly
ggplot(mtcars) + geom_point(aes(x = mpg, y = disp))

# The 'y' values for the histogram are computed by the stat
ggplot(faithful, aes(x = waiting)) +
  geom_histogram()
# Choosing a different computed variable to display, matching up the
# histogram with the density plot
ggplot(faithful, aes(x = waiting)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density()

# The exact colour is known after scale transformation
ggplot(mpg, aes(cty, colour = factor(cyl))) +
  geom_density()
# We re-use colour properties for the fill without a separate fill scale
ggplot(mpg, aes(cty, colour = factor(cyl))) +
  geom_density(aes(fill = after_scale(alpha(colour, 0.3))))

# Use stage to modify the scaled fill
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(fill = stage(class, after_scale = alpha(fill, 0.4))))
# Using data for computing summary, but placing label elsewhere.
# Also, we're making our own computed variable to use for the label.
ggplot(mpg, aes(class, displ)) +
  geom_violin() +
  stat_summary(
    aes(
      y = stage(displ, after_stat = 8),
      label = after_stat(paste(mean, "±", sd))
    ),
    geom = "text",
    fun.data = ~ round(data.frame(mean = mean(.x), sd = sd(.x)), 2)
  )

# Default histogram display
ggplot(mpg, aes(displ)) +
  geom_histogram(aes(y = after_stat(count)))
# Scale tallest bin to 1
ggplot(mpg, aes(displ)) +
  geom_histogram(aes(y = after_stat(count / max(count))))
# Use a transparent version of colour for fill
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(colour = class, fill = after_scale(alpha(colour, 0.4))))
# Use stage to modify the scaled fill
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(fill = stage(class, after_scale = alpha(fill, 0.4))))
# Making a proportional stacked density plot
ggplot(mpg, aes(cty)) +
  geom_density(
    aes(
      colour = factor(cyl),
      fill = after_scale(alpha(colour, 0.3)),
      y = after_stat(count / sum(n[!duplicated(group)]))
    ),
    position = "stack", bw = 1
  ) +
  geom_density(bw = 1)
# Imitating a ridgeline plot
ggplot(mpg, aes(cty, colour = factor(cyl))) +
  geom_ribbon(
    stat = "density", outline.type = "upper",
    aes(
      fill = after_scale(alpha(colour, 0.3)),
      ymin = after_stat(group),
      ymax = after_stat(group + ndensity)
    )
  )
# Labelling a bar plot
ggplot(mpg, aes(class)) +
  geom_bar() +
  geom_text(
    aes(
      y = after_stat(count + 2),
      label = after_stat(count)
    ),
    stat = "count"
  )
# Labelling the upper hinge of a boxplot,
# inspired by June Choe
ggplot(mpg, aes(displ, class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(
    aes(
      label = after_stat(xmax),
      x = stage(displ, after_stat = xmax)
    ),
    stat = "boxplot", hjust =-0.5
  )


p <- ggplot(mtcars, aes(wt, mpg))
# A basic scatter plot
p + geom_point(size = 4)
# Using the colour aesthetic
p + geom_point(aes(colour = factor(cyl)), size = 4)
# Using the shape aesthetic
p + geom_point(aes(shape = factor(cyl)), size = 4)
# Using fill
p <- ggplot(mtcars, aes(factor(cyl)))
p + geom_bar()
p + geom_bar(aes(fill = factor(cyl)))
p + geom_bar(aes(fill = factor(vs)))
# Using linetypes
ggplot(economics_long, aes(date, value01)) +
  geom_line(aes(linetype = variable))

library(nlme)
# Multiple groups with one aesthetic
p <- ggplot(nlme::Oxboys, aes(age, height))
# The default is not sufficient here. A single line tries to connect all
# the observations.
p + geom_line()
# To fix this, use the group aesthetic to map a different line for each
# subject.
p + geom_line(aes(group = Subject))

# Different groups on different layers
p <- p + geom_line(aes(group = Subject))
# Using the group aesthetic with both geom_line() and geom_smooth()
# groups the data the same way for both layers
p + geom_smooth(aes(group = Subject), method = "lm", se = FALSE)
# Changing the group aesthetic for the smoother layer
# fits a single line of best fit across all boys
p + geom_smooth(aes(group = 1), size = 2, method = "lm", se = FALSE)

# Overriding the default grouping
# Sometimes the plot has a discrete scale but you want to draw lines
# that connect across groups. This is the strategy used in interaction
# plots, profile plots, and parallel coordinate plots, among others.
# For example, we draw boxplots of height at each measurement occasion.
p <- ggplot(nlme::Oxboys, aes(Occasion, height)) + geom_boxplot()
p
# There is no need to specify the group aesthetic here; the default grouping
# works because occasion is a discrete variable. To overlay individual
# trajectories, we again need to override the default grouping for that layer
# with aes(group = Subject)
p +  geom_line(aes(group = Subject), colour = "blue")


df <- data.frame(x = 1:10 , y = 1:10)
p <- ggplot(df, aes(x, y))
p + geom_line(linetype = 2)
p + geom_line(linetype = "dotdash")
# An example with hex strings; the string "33" specifies three units on followed
# by three off and "3313" specifies three units on followed by three off followed
# by one on and finally three off.
p + geom_line(linetype = "3313")
# Mapping line type from a grouping variable
ggplot(economics_long, aes(date, value01)) +
  geom_line(aes(linetype = variable))
# Linewidth examples
ggplot(economics, aes(date, unemploy)) +
  geom_line(linewidth = 2, lineend = "round")
ggplot(economics, aes(date, unemploy)) +
  geom_line(aes(linewidth = uempmed), lineend = "round")
# Size examples
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(size = 4)
p + geom_point(aes(size = qsec))
p + geom_point(size = 2.5) +
  geom_hline(yintercept = 25, size = 3.5)
# Shape examples
p + geom_point()
p + geom_point(shape = 5)
p + geom_point(shape = "k", size = 3)
p + geom_point(shape = ".")
p + geom_point(shape = NA)
p + geom_point(aes(shape = factor(cyl)))
# A look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
p <- ggplot(df2, aes(x, y))
p + geom_point(aes(shape = z), size = 4) +
  scale_shape_identity()
# While all symbols have a foreground colour, symbols 19-25 also take a
# background colour (fill)
p + geom_point(aes(shape = z), size = 4, colour = "red") +
  scale_shape_identity()
p + geom_point(aes(shape = z), size = 4, colour = "red", fill = "black") +
  scale_shape_identity()

# Generate data: means and standard errors of means for prices
# for each type of cut
dmod <- lm(price ~ cut, data = diamonds)
cut <- unique(diamonds$cut)
cuts_df <- data.frame(
  cut,
  predict(dmod, data.frame(cut), se = TRUE)[c("fit", "se.fit")]
)
ggplot(cuts_df) +
  aes(
    x = cut,
    y = fit,
    ymin = fit- se.fit,
    ymax = fit + se.fit,
    colour = cut
  ) +
  geom_pointrange()

# Using annotate
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p
p + annotate(
  "rect", xmin = 2, xmax = 3.5, ymin = 2, ymax = 25,
  fill = "dark grey", alpha = .5
)

# Geom_segment examples
p + geom_segment(
  aes(x = 2, y = 15, xend = 2, yend = 25),
  arrow = arrow(length = unit(0.5, "cm"))
)
p + geom_segment(
  aes(x = 2, y = 15, xend = 3, yend = 15),
  arrow = arrow(length = unit(0.5, "cm"))
)
p + geom_segment(
  aes(x = 5, y = 30, xend = 3.5, yend = 25),
  arrow = arrow(length = unit(0.5, "cm"))
)

# You can also use geom_segment() to recreate plot(type = "h")
# from base R:
set.seed(1)
counts <- as.data.frame(table(x = rpois(100, 5)))
counts$x <- as.numeric(as.character(counts$x))
with(counts, plot(x, Freq, type = "h", lwd = 10))
ggplot(counts, aes(x = x, y = Freq)) +
  geom_segment(aes(yend = 0, xend = x), size = 10)

# Annotate
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 4, y = 25, label = "Some text")
p + annotate("text", x = 2:5, y = 25, label = "Some text")
p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,
             alpha = .2)
p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,
             colour = "blue")
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
             colour = "red", size = 2.5, linewidth = 1.5)
p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
p + annotate("text", x = 4, y = 25, label = "italic(R) ^ 2 == 0.75",
             parse = TRUE)
p + annotate("text", x = 4, y = 25,
             label = "paste(italic(R) ^ 2, \" = .75\")", parse = TRUE)

# Dummy plot
df <- data.frame(x = 1:10, y = 1:10)
base <- ggplot(df, aes(x, y)) +
  geom_blank() +
  theme_bw()
# Full panel annotation
base + annotation_custom(
  grob = grid::roundrectGrob(),
  xmin =-Inf, xmax = Inf, ymin =-Inf, ymax = Inf
)
# Inset plot
df2 <- data.frame(x = 1 , y = 1)
g <- ggplotGrob(ggplot(df2, aes(x, y)) +
                  geom_point() +
                  theme(plot.background = element_rect(colour = "black")))
base +
  annotation_custom(grob = g, xmin = 1, xmax = 10, ymin = 8, ymax = 10)

# Make a log-log plot (without log ticks)
a <- ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point(na.rm = TRUE) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_bw()

a + annotation_logticks()
a + annotation_logticks(sides = "lr")
# Default: log ticks on bottom and left
# Log ticks for y, on left and right
a + annotation_logticks(sides = "trbl") # All four sides
a + annotation_logticks(sides = "lr", outside = TRUE) +
  coord_cartesian(clip = "off") # Ticks outside plot

# Hide the minor grid lines because they don't align with the ticks
a + annotation_logticks(sides = "trbl") + 
  theme(panel.grid.minor = element_blank())
# Another way to get the same results as 'a' above: log-transform the data before
# plotting it. Also hide the minor grid lines.
b <- ggplot(msleep, aes(log10(bodywt), log10(brainwt))) +
  geom_point(na.rm = TRUE) +
  scale_x_continuous(name = "body", labels = scales::label_math(10^.x)) +
  scale_y_continuous(name = "brain", labels = scales::label_math(10^.x)) +
  theme_bw() + theme(panel.grid.minor = element_blank())
b + annotation_logticks()
# Using a coordinate transform requires scaled = FALSE
t <- ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10") +
  theme_bw()
t + annotation_logticks(scaled = FALSE)
# Change the length of the ticks
a + annotation_logticks(
  short = unit(.5,"mm"),
  mid = unit(3,"mm"),
  long = unit(4,"mm")
)

