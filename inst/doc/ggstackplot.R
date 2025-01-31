## ----include = FALSE----------------------------------------------------------
# once in quarto this can go into the front matter
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
# load the package
library(ggstackplot)

## -----------------------------------------------------------------------------
# select any number of variables to make the stack
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec, drat)
  )

# the selection order translates into stack order
mtcars |> 
  ggstackplot(
    x = mpg, y = c(drat, wt, qsec)
  )

# use any valid tidyselect selection syntax
mtcars |> 
  ggstackplot(
    x = mpg, y = c(4, "carb", starts_with("d"))
  )

# use any valid tidyselect renaming syntax to rename stack panels
mtcars |> 
  ggstackplot(
    x = c(`mpg [units]` = mpg), 
    y = c(`weight [tons]` = wt, `speed` = qsec, drat)
  )

## -----------------------------------------------------------------------------
# all examples shown in this document work the same way for a horizontal
# stack, simply switch out the x and y assignments
mtcars |> 
  ggstackplot(
    y = mpg, x = c(wt, qsec, drat)
  )

## -----------------------------------------------------------------------------
# use the Set1 RColorBrewer palette
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    palette = "Set1"
  )

## -----------------------------------------------------------------------------
# likewise for the horizontal stack version
mtcars |> 
  ggstackplot(
    y = mpg, x = c(wt, qsec),
    palette = "Set1"
  )

## -----------------------------------------------------------------------------
# select any specific colors for each plot
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8")
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)

# default (NAs are removed so lines are not interrupted)
mtcars |> 
  add_row(mpg = 22, wt = 5, qsec = NA) |>
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8")
  )

# explicit `remove_na` = FALSE
mtcars |> 
  add_row(mpg = 22, wt = 5, qsec = NA) |>
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    remove_na = FALSE
  )

## -----------------------------------------------------------------------------
# Vertical stackplot
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    both_axes = TRUE
  )

# Horizontal stackplot
mtcars |> 
  ggstackplot(
    y = mpg, x = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    both_axes = TRUE
  )

## -----------------------------------------------------------------------------
# axes do not alternate:
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    alternate_axes = FALSE
  )

# Horizontal version
mtcars |> 
  ggstackplot(
    y = mpg, x = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    alternate_axes = FALSE
  )

## -----------------------------------------------------------------------------
# stacked axis starts on the right
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    switch_axes = TRUE
  )

# or for the horizontal version, stacked axis
# starts on the bottom
mtcars |> 
  ggstackplot(
    y = mpg, x = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    switch_axes = TRUE
  )

# and in combination with alternate_axes = FALSE
# all axes on the right
mtcars |> 
  ggstackplot(
    x = mpg, y = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    alternate_axes = FALSE,
    switch_axes = TRUE
  )

# or all axes on the top
mtcars |> 
  ggstackplot(
    y = mpg, x = c(wt, qsec),
    color = c("#E41A1C", "#377EB8"),
    alternate_axes = FALSE,
    switch_axes = TRUE
  )

## -----------------------------------------------------------------------------
# define any overlap between 0 and 1
mtcars |> 
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 0.3
  )

# full overlap
mtcars |> 
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 1
  )

## -----------------------------------------------------------------------------
# different overlap between stack panels
mtcars |> 
  ggstackplot(
    x = mpg, 
    y = c(qsec, drat, wt, hp),
    color = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
    overlap = c(1, 0, 1)
  )

# and the horizontal version
mtcars |> 
  ggstackplot(
    y = mpg, 
    x = c(qsec, drat, wt, hp),
    color = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
    overlap = c(1, 0, 1)
  )

## -----------------------------------------------------------------------------
mtcars |> 
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 1,
    # can be only 10% of a plot size as we're overlapping plots
    shared_axis_size = 1
  )

## -----------------------------------------------------------------------------
mtcars |> 
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    simplify_shared_axis = FALSE
  )

# also goes well with changing `both_axes`, `switch_axes` and/or `alternate_axes`
mtcars |> 
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    simplify_shared_axis = FALSE,
    alternate_axes = FALSE
  )

## -----------------------------------------------------------------------------
library(ggplot2)

# increase y axis text size
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    template = 
      ggplot() + 
      geom_line() +
      theme_stackplot() +
      theme(
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16)
      )
  )

# increase the panel margins
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    template = 
      ggplot() + 
      geom_line() +
      theme_stackplot() +
      theme(
        # increase left margin to 20% and top/bottom margins to 10%
        plot.margin = margin(l = 0.2, t = 0.1, b = 0.1, unit = "npc")
      )
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 1,
    template = ggplot() +
      geom_line(data = function(df) filter(df, .yvar == "qsec")) +
      geom_point(data = function(df) filter(df, .yvar == "drat")) +
      theme_stackplot() +
      theme(
        panel.grid.major = element_line(
          color = "lightgray", 
          linewidth = 0.8)
      )
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 0,
    template = ggplot() +
      geom_line(data = function(df) filter(df, .yvar == "qsec")) +
      geom_point(data = function(df) filter(df, .yvar == "drat")) +
      theme_stackplot() +
      theme(
        panel.grid.major = element_line(
          color = "lightgray", 
          linetype = "dotted", 
          linewidth = 0.5)
      )
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 0,
    template = ggplot() +
      geom_line(data = function(df) filter(df, .yvar == "qsec")) +
      geom_point(data = function(df) filter(df, .yvar == "drat")) +
      theme_stackplot() +
      theme_bw() # give us that good theme!
  )

## -----------------------------------------------------------------------------
# use different geoms for different panels
# you can refer to y-stack panel variables with `.yvar` and x-stack panel variables with `.xvar`
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 1,
    template = ggplot() +
      geom_line(data = function(df) filter(df, .yvar == "qsec")) +
      geom_point(data = function(df) filter(df, .yvar == "drat")) +
      theme_stackplot()
  )

## -----------------------------------------------------------------------------
# horizontal stack with default (geom_line())
mtcars |>
  ggstackplot(
    y = mpg, x = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    template = 
      ggplot() + 
      geom_point() +
      geom_line() + # default in template
      theme_stackplot() 
  )
# the following is the exact same data but using a
# horizontal stack with "depth-profile" like geom_path()
mtcars |>
  # arrange data by the y-axis
  arrange(mpg) |>
  ggstackplot(
    y = mpg, x = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    template = 
      ggplot() + 
      geom_point() +
      geom_path() + # plots data in order
      theme_stackplot() 
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    overlap = 0.2, 
    template = 
      ggplot() + 
      geom_vline(xintercept = 20, linewidth = 4, color = "gray80") +
      geom_line() +
      theme_stackplot() 
  )

## -----------------------------------------------------------------------------
# add a secondary x axis
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    both_axes = TRUE, overlap = 0.1, 
    template = 
      ggplot() + 
      geom_line() +
      scale_x_continuous(
        # change axis name
        name = "this is my mpg axis",
        # this can be the same with dup_axis() or as here have a transformed axis
        sec.axis = sec_axis(
          transform = sqrt, 
          name = expression(sqrt(mpg)), 
          breaks = scales::pretty_breaks(5)
        )
      ) +
      theme_stackplot() 
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    both_axes = TRUE, overlap = 0.1,
    template = 
      ggplot() + 
      geom_line() +
      scale_x_log10("this is my log10 mpg axis") +
      theme_stackplot() 
  )

## -----------------------------------------------------------------------------
# add aesthetics to the plot
mtcars |>
  ggstackplot(
    x = mpg,  y = c(wt, qsec, drat),
    alternate_axes = FALSE,
    template = 
      ggplot() +
      aes(color = factor(cyl), linetype = factor(cyl), shape = factor(cyl)) +
      geom_line() +
      geom_point(size = 3) +
      theme_stackplot() 
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    template = ggplot() + theme_stackplot(),
    # add:
    add = list(
      # panel by name
      qsec = geom_line(), 
      drat = geom_rect(
        xmin = 20, xmax = 25, ymin = 3.2, ymax = 4.2, fill = "gray90") + 
        geom_point()
    )
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    # define ggplot template options
    template = 
      ggplot() + 
      geom_line() + 
      theme_stackplot(),
    # define panel-specific additions
    add = list(
      # panel by index
      # first panel:
      geom_point() + theme(
        axis.title.y = element_text(size = 30)),
      # second panel:
      theme(
        panel.grid.major.y = element_line(
          color = "lightgray", 
          size = 0.2))
    )
  )

## ----message=FALSE------------------------------------------------------------
# particularly useful is also the possibility to modify individual scales
mtcars |>
  ggstackplot(
    x = mpg, y = c(qsec, drat),
    color = c("#E41A1C", "#377EB8"),
    template = ggplot() + geom_line() + theme_stackplot(),
    add = list(
      # modify the axis for the second plot
      drat = 
        scale_y_continuous("$$ drat",  labels = scales::label_dollar()) + 
        expand_limits(y = 0) +
        theme(axis.title.y = element_text(size = 30))
    )
  )

## -----------------------------------------------------------------------------
mtcars |>
  ggstackplot(
    x = mpg,  y = c(wt, qsec, drat),
    color = c("#E41A1C", "#377EB8", "#4DAF4A"),
    template = 
      ggplot() + aes(linetype = factor(vs)) +
      geom_line() + theme_stackplot(),
    # switch legend position for middle plot
    add = list(qsec = theme(legend.position = "left"))
  )

mtcars |>
  ggstackplot(
    x = mpg,  y = c(wt, qsec, drat),
    color = c("#E41A1C", "#377EB8", "#4DAF4A"),
    template = 
      ggplot() + 
      aes(linetype = factor(vs)) +
      geom_line() + 
      theme_stackplot() +
      # remove the legends, then...
      theme(legend.position = "none"), 
    # ... re-include the middle panel legend on the plot
    # with some additional styling
    add = list(
      qsec = 
        theme(
          # define legend relative position in x,y:
          legend.position = c(0.2, 0.9), 
          # other legend stylistic changes:
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16),
          legend.background = element_rect(
            color = "black", fill = "gray90", linewidth = 0.5),
          legend.key = element_blank(),
          legend.direction = "horizontal"
        ) +
        labs(linetype = "VS")
    )
  )

## ----message = FALSE, fig.height=8--------------------------------------------
# example from the README with economics data bundled with ggplot2
ggplot2::economics |>
  ggstackplot(
    # define shared x axis
    x = date, 
    # define the stacked y axes
    y = c(pce, pop, psavert, unemploy),
    # pick the RColorBrewer Dark2 palette (good color contrast)
    palette = "Dark2",
    # overlay the pce & pop plots (1), then make a full break (0) to the once
    # again overlaye psavert & unemploy plots (1)
    overlap = c(1, 0, 1),
    # switch axes so unemploy and psavert are on the side where they are 
    # highest, respectively - not doing this here by changing the order of y
    # because we want pop and unemploy on the same side
    switch_axes = TRUE,
    # make shared axis space a bit smaller
    shared_axis_size = 0.15,
    # provide a base plot with shared graphics eelements among all plots
    template = 
      # it's a ggplot
      ggplot() +
      # use a line plot for all
      geom_line() +
      # we want the default stackplot theme
      theme_stackplot() +
      # add custom theme modifications, such as text size
      theme(text = element_text(size = 14)) +
      # make the shared axis a date axis
      scale_x_date("year") +
      # include y=0 for all plots to contextualize data better
      expand_limits(y = 0),
    # add plot specific elements
    add = 
      list(
        pce = 
          # show pce in trillions of dollars
          scale_y_continuous(
            "personal consumption expenditures", 
            # always keep the secondary axis duplicated so ggstackplot can
            # manage axis placement for you
            sec.axis = dup_axis(),
            # labeling function for the dollar units
            labels = function(x) sprintf("$%.1f T", x/1000),
          ),
        pop = 
          # show population in millions
          scale_y_continuous(
            "population", sec.axis = dup_axis(),
            labels = function(x) sprintf("%.0f M", x/1000)
          ),
        psavert = 
          # savings is in %
          scale_y_continuous(
            "personal savings rate", sec.axis = dup_axis(),
            labels = function(x) paste0(x, "%"),
          ) +
          # show data points in addition to line
          geom_point(),
        unemploy = 
          # unemploy in millions
          scale_y_continuous(
            "unemployed persons", sec.axis = dup_axis(),
            labels = function(x) sprintf("%.0f M", x/1000)
          ) +
          # show data points in addition to line
          geom_point()
      )
  )

## -----------------------------------------------------------------------------
# prep plot
plot_prep <- 
  mtcars |> 
  prepare_stackplot(
    x = mpg, y = c(wt, qsec),
    palette = "Set1"
  )

# show plot tibble
plot_prep

# modify plot tibble
plot_prep$plot[[2]] <- ggplot(mtcars) + aes(mpg, drat) + geom_point()
plot_prep$theme[[2]] <- theme_bw()

# assemble stackplot
plot_prep |> assemble_stackplot()

