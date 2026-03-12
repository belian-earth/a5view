library(shiny)
library(a5R)
devtools::load_all()
# library(a5view)

resolution <- 8

# Initial centre: Edinburgh
centre <- a5_lonlat_to_cell(-3.19, 55.95, resolution = resolution)

#' Compute spherical cap cells with distances
compute_cap <- function(cell, radius_km, uncompact = TRUE) {
  cap_cells <- a5_spherical_cap(cell, radius_km * 1000)
  if (uncompact) {
    cap_cells <- a5_uncompact(cap_cells, resolution = resolution)
  }
  if (length(cap_cells) == 0) {
    return(NULL)
  }
  dist_km <- as.numeric(a5_cell_distance(cell, cap_cells, units = "km"))
  # Fall back to NULL fill when colour mapping isn't possible
  if (length(dist_km) < 2 || diff(range(dist_km)) == 0) {
    return(list(cells = cap_cells, dist = NULL))
  }
  list(cells = cap_cells, dist = dist_km)
}

ui <- fluidPage(
  tags$head(tags$style(HTML(
    "
    html, body { margin: 0; padding: 0; height: 100%; overflow: hidden; }
    .container-fluid { padding: 0; height: 100%; }
    #controls {
      position: absolute; bottom: 20px; left: 50%; transform: translateX(-50%);
      z-index: 1000;
      background: rgba(30, 30, 30, 0.75);
      backdrop-filter: blur(12px);
      -webkit-backdrop-filter: blur(12px);
      border: 1px solid rgba(116, 172, 144, 0.4);
      border-radius: 12px;
      padding: 12px 20px;
      color: #e0e0e0;
      font-family: system-ui, sans-serif;
      font-size: 13px;
      display: flex; align-items: center; gap: 16px;
    }
    #controls label { margin: 0; color: #74ac90; font-weight: 500; }
    #controls input[type=range] {
      width: 160px; accent-color: #74ac90; vertical-align: middle;
    }
    #controls .val { min-width: 50px; text-align: right; }
    #controls .divider {
      width: 1px; height: 20px;
      background: rgba(116, 172, 144, 0.4);
    }
    #controls input[type=checkbox] { accent-color: #74ac90; }
  "
  ))),

  div(
    id = "controls",
    tags$label("Radius (km)"),
    tags$input(
      id = "radius_slider",
      type = "range",
      min = 30,
      max = 2000,
      value = 50,
      step = 10
    ),
    tags$span(id = "radius_val", class = "val", "50 km"),
    tags$span(class = "divider"),
    tags$label(`for` = "uncompact_cb", "Uncompact"),
    tags$input(
      id = "uncompact_cb",
      type = "checkbox",
      checked = "checked"
    ),
    tags$span(class = "divider"),
    tags$label(`for` = "globe_cb", "Globe"),
    tags$input(id = "globe_cb", type = "checkbox"),
    tags$script(HTML(
      "
      document.getElementById('radius_slider').addEventListener('input', function() {
        var v = this.value;
        document.getElementById('radius_val').textContent = v + ' km';
        Shiny.setInputValue('radius_km', parseInt(v));
      });
      document.getElementById('uncompact_cb').addEventListener('change', function() {
        Shiny.setInputValue('uncompact', this.checked);
      });
      document.getElementById('globe_cb').addEventListener('change', function() {
        Shiny.setInputValue('globe', this.checked);
      });
      $(document).on('shiny:connected', function() {
        Shiny.setInputValue('radius_km', 50);
        Shiny.setInputValue('uncompact', true);
        Shiny.setInputValue('globe', false);
      });
    "
    ))
  ),

  a5_viewOutput("map", width = "100%", height = "100vh")
)

server <- function(input, output, session) {
  # Track the current centre cell — start with Edinburgh
  current_centre <- reactiveVal(centre)

  # Whether the cap is pinned (clicked) or following the cursor
  pinned <- reactiveVal(FALSE)

  # Debounce cursor lat/lon so we only recompute after it settles
  cursor <- reactive(input$map_cursor) |> debounce(10)

  # Hover updates centre only when not pinned
  observeEvent(cursor(), {
    if (!pinned()) {
      coords <- cursor()
      if (!is.null(coords)) {
        current_centre(a5_lonlat_to_cell(
          coords$lng,
          coords$lat,
          resolution = resolution
        ))
      }
    }
  })

  # Click toggles pin: first click pins, second click unpins
  observeEvent(input$map_click_coord, {
    if (pinned()) {
      pinned(FALSE)
    } else {
      coords <- input$map_click_coord
      if (!is.null(coords)) {
        current_centre(a5_lonlat_to_cell(
          coords$lng,
          coords$lat,
          resolution = resolution
        ))
        pinned(TRUE)
      }
    }
  })

  cap_data <- reactive({
    radius <- input$radius_km
    if (is.null(radius)) {
      radius <- 50
    }
    uc <- isTRUE(input$uncompact)
    compute_cap(current_centre(), radius, uncompact = uc)
  })

  output$map <- renderA5_view({
    cap <- cap_data()
    if (is.null(cap)) {
      return(NULL)
    }

    args <- list(
      cells = cap$cells,
      tooltip = pinned(),
      border = NULL,
      globe = isTRUE(input$globe),
      basemap = "dark",
      opacity = 0.9
    )
    if (!is.null(cap$dist)) {
      args$fill <- cap$dist
      args$palette <- "Inferno"
    }
    do.call(a5_view, args)
  })
}

shinyApp(ui, server)
