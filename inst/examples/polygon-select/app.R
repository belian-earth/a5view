library(shiny)
library(a5R)
library(a5view)

a5_set_threads(4)

resolution <- 4L

# Initial map: a res-4 grid over Europe so the user has cells to draw over.
init_cells <- a5_grid(c(-25, 30, 45, 70), resolution = resolution)

FILL_SELECTED   <- "#74ac90ff"
FILL_UNSELECTED <- "#74ac901a"

selection_fills <- function(cells, sel) {
  ifelse(cells %in% sel, FILL_SELECTED, FILL_UNSELECTED)
}

ui <- fluidPage(
  tags$head(tags$style(HTML(
    "
    html, body {
      margin: 0; padding: 0; height: 100%; overflow: hidden;
      font-family: 'Inter', system-ui, -apple-system, sans-serif;
    }
    .container-fluid { padding: 0; height: 100vh; max-width: none; }
    .layout { display: flex; height: 100vh; width: 100vw; }
    .map-pane { flex: 1; min-width: 0; height: 100%; }
    .list-pane {
      width: 320px; flex: 0 0 320px; box-sizing: border-box;
      background: #1b1b1b; color: #ddd;
      padding: 20px; overflow-y: auto;
      border-left: 1px solid rgba(116, 172, 144, 0.25);
    }
    .list-pane h3 {
      color: #74ac90; font-weight: 500; font-size: 13px;
      letter-spacing: 0.4px; text-transform: uppercase;
      margin: 0 0 6px;
    }
    .list-pane .hint { color: #888; font-size: 12px; margin-bottom: 14px; line-height: 1.5; }
    .list-pane ul { list-style: none; padding: 0; margin: 0; }
    .list-pane li {
      font-family: ui-monospace, 'JetBrains Mono', Menlo, monospace;
      font-size: 12px; padding: 5px 8px; color: #ccc;
      border-radius: 4px;
    }
    .list-pane li:hover { background: rgba(116, 172, 144, 0.12); color: #fff; }
    .list-pane .empty { color: #777; font-style: italic; font-size: 12px; }
    .list-pane .actions { margin-bottom: 14px; }
    .list-pane button#clear {
      background: rgba(116, 172, 144, 0.12);
      color: #74ac90; border: 1px solid rgba(116, 172, 144, 0.4);
      border-radius: 6px; padding: 5px 12px; font-size: 12px;
      cursor: pointer; font-family: inherit;
      transition: background 0.15s, color 0.15s;
    }
    .list-pane button#clear:hover { background: rgba(116, 172, 144, 0.25); color: #fff; }
    .list-pane button#clear:disabled { opacity: 0.4; cursor: default; }
    "
  ))),

  div(
    class = "layout",
    div(class = "map-pane", a5_viewOutput("map", width = "100%", height = "100%")),
    div(
      class = "list-pane",
      h3("Selected cells"),
      div(class = "hint", textOutput("count", inline = TRUE)),
      div(class = "actions", actionButton("clear", "Clear")),
      uiOutput("cell_list")
    )
  )
)

server <- function(input, output, session) {
  selected <- reactiveVal(a5_cell(character(0)))

  output$map <- renderA5_view({
    a5_view(
      init_cells,
      fill = FILL_UNSELECTED,
      border = "#74ac9080",
      opacity = 0.7,
      tooltip = FALSE,
      draw_polygon = TRUE,
      lng = 10, lat = 50, zoom = 3
    )
  })

  # Push per-cell fills back to the widget every time the selection
  # changes so selected cells stand out visually.
  observeEvent(selected(), ignoreInit = TRUE, {
    a5_view_update(
      session,
      "map",
      init_cells,
      fill = selection_fills(init_cells, selected()),
      tooltip = FALSE
    )
  })

  # Single-cell click: toggle the cell at the click location. Uses
  # _click_coord (suppressed automatically while draw mode is on) and
  # resolves the cell server-side, so this doesn't depend on the JS
  # widget's own click-highlight toggle.
  observeEvent(input$map_click_coord, {
    coords <- input$map_click_coord
    if (is.null(coords)) return()
    cell <- a5_lonlat_to_cell(coords$lng, coords$lat, resolution = resolution)
    cur <- selected()
    if (cell %in% cur) {
      selected(cur[cur != cell])
    } else {
      selected(c(cur, cell))
    }
  })

  # Polygon: union into the existing selection.
  observeEvent(input$map_polygon_draw, {
    wkt <- input$map_polygon_draw
    new_cells <- tryCatch(
      a5_grid(wk::wkt(wkt), resolution = resolution),
      error = function(e) {
        showNotification(
          paste("a5_grid failed:", conditionMessage(e)),
          type = "error"
        )
        a5_cell(character(0))
      }
    )
    selected(unique(c(selected(), new_cells)))
  })

  observeEvent(input$clear, {
    selected(a5_cell(character(0)))
  })

  output$count <- renderText({
    n <- length(selected())
    if (n == 0L) {
      "Click a cell to select it, or use the draw-polygon toolbar button to select an area."
    } else {
      sprintf("%d cell%s at resolution %d", n, if (n == 1L) "" else "s", resolution)
    }
  })

  output$cell_list <- renderUI({
    cells <- selected()
    if (length(cells) == 0L) {
      div(class = "empty", "(none yet)")
    } else {
      ids <- format(cells)
      tags$ul(lapply(ids, function(id) tags$li(id)))
    }
  })
}

shinyApp(ui, server)
