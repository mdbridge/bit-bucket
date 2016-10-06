### 
### Controlling Excel through VBA: selected operations on chart (part)s
### 

import win32com.client


## 
## Common code and selected Excel constants
## 

XlCategory = 1            # axis displays categories (usually X)
xlValue    = 2            # axis displays values     (usually Y)

xlPrimary  = 1            # primary axis group

xlTickMarkCross   = 4     # Crosses the axis
xlTickMarkInside  = 2     # Inside the axis
xlTickMarkNone    = -4142 # No mark
xlTickMarkOutside = 3     # Outside the axis


def open_Excel():
    # attempt to connect to currently open and in focus Excel
    # application instance:
    return win32com.client.GetObject(Class="Excel.Application")

def open_Excel_with_active_chart():
    xlApp = open_Excel()
    if xlApp.ActiveChart == None:
        # If no chart currently active, try and use first embedded
        # chart on active sheet:
        xlApp.ActiveSheet.ChartObjects(1).Activate()
    return xlApp

def RGB(red,green,blue):
    return red*256*256 + green*256 + blue



## 
## Move currently selected chart so its top is at (x,y):
## 

# Vocola procedure: Excel.MoveChart
def move_chart(x, y):
    xlApp = open_Excel_with_active_chart()
    xlApp.ActiveChart.Parent.Left = int(x)
    xlApp.ActiveChart.Parent.Top  = int(y)


## 
## Select the given chart part, creating it first if needed:
## 

# Vocola procedure: Excel.GoChartPart
def go_chart_part(part):
    xlApp = open_Excel_with_active_chart()
    if part == "title":
        xlApp.ActiveChart.HasTitle = 1
        xlApp.ActiveChart.ChartTitle.Select()
    elif part == "X":
        xlApp.ActiveChart.SetHasAxis(XlCategory, xlPrimary, 1)
        xlApp.ActiveChart.Axes(XlCategory).Select()
    elif part == "Y":
        xlApp.ActiveChart.SetHasAxis(xlValue, xlPrimary, 1)
        xlApp.ActiveChart.Axes(xlValue).Select()
    elif part == "X title":
        xlApp.ActiveChart.Axes(XlCategory).HasTitle = 1
        xlApp.ActiveChart.Axes(XlCategory).AxisTitle.Select()        
    elif part == "Y title":
        xlApp.ActiveChart.Axes(xlValue).HasTitle = 1
        xlApp.ActiveChart.Axes(xlValue).AxisTitle.Select()        
    elif part == "legend":
        xlApp.ActiveChart.HasLegend = 1
        xlApp.ActiveChart.Legend.Select()
    elif part == "plot area":
        xlApp.ActiveChart.PlotArea.Select()
    elif part == "chart area":
        xlApp.ActiveChart.ChartArea.Select()
    else:
        raise ValueError("unknown chart part: " + part)


## 
## Miscellaneous functions for setting various chart properties:
## 

# precondition: select a data series in a chart
# Vocola procedure: Excel.SetSeriesLineWidth
def set_series_line_width(width):
    xlApp = open_Excel()
    f     = xlApp.Selection.Format
    f.Line.Weight  = float(width)

# precondition: select a data series in a chart
# Vocola procedure: Excel.SetMarkerSize
def set_marker_size(size):
    xlApp = open_Excel()
    f     = xlApp.Selection
    f.MarkerSize = int(size)

# precondition: select a data series in a chart
# Vocola procedure: Excel.AddTrendline
def add_trendline():
    xlApp = open_Excel()
    xlApp.ActiveChart.SetElement(601) # msoElementTrendlineAddLinear
    xlApp.Selection.Trendlines(1).Select()
    f = xlApp.Selection
    f.DisplayEquation = 1
    f.DisplayRSquared = 1


# Vocola procedure: Excel.SetTickInterval
def set_tick_interval(interval):
    xlApp = open_Excel_with_active_chart()
    interval = int(interval)
    if interval > 0:
        xlApp.ActiveChart.Axes(XlCategory).MajorTickMark   = xlTickMarkOutside
        xlApp.ActiveChart.Axes(XlCategory).TickMarkSpacing = interval
    else:
        xlApp.ActiveChart.Axes(XlCategory).TickMarkSpacing = 1
        xlApp.ActiveChart.Axes(XlCategory).MajorTickMark   = xlTickMarkNone

# Vocola procedure: Excel.SetLabelInterval
def set_label_interval(interval):
    xlApp = open_Excel_with_active_chart()
    interval = int(interval)
    if interval > 0:
        xlApp.ActiveChart.Axes(XlCategory).TickLabelSpacing = interval
    else:
        xlApp.ActiveChart.Axes(XlCategory).TickLabelSpacingIsAuto = 1

# Vocola procedure: Excel.MajorGridlinesGrey
def major_gridlines_grey(grey_level):
    xlApp = open_Excel_with_active_chart()
    f     = xlApp.ActiveChart.Axes(xlValue).MajorGridlines.Format
    f.Line.Visible = 1
    g = int(grey_level)
    f.Line.ForeColor.RGB = RGB(g,g,g)

# Vocola procedure: Excel.OutlineLegend
def outline_legend():
    xlApp = open_Excel_with_active_chart()
    f     = xlApp.ActiveChart.Legend.Format
    f.Fill.Solid()
    f.Fill.ForeColor.RGB = RGB(255,255,255) # that is, white
    f.Line.ForeColor.RGB = RGB(0, 0, 0)     # that is, black

# Vocola procedure: Excel.PlotAreaBorder
def plot_area_border(present):
    xlApp = open_Excel_with_active_chart()
    if int(present) != 0:
        xlApp.ActiveChart.PlotArea.Format.Line.Visible = 1
    else:
        xlApp.ActiveChart.PlotArea.Format.Line.Visible = 0

# Vocola procedure: Excel.NoChartBorder
def no_chart_border():
    xlApp = open_Excel_with_active_chart()
    xlApp.ActiveChart.ChartArea.Format.Line.Visible = 0
