# Randomly rotate and move protected area polygons within study area polygon

# Load required libraries:
library('sf')

# Get the operating system file separator. This will probably always be '/':
fs <- .Platform$file.sep;

# ---

# Directory containing protected areas shapefile:
pa_dir <- 'protected_areas'

# Directory containing study area shapefile:
sa_dir <- 'study_area'

# Number of iterations of moving and rotating to run:
n_iters <- 30

# Output directory:
out_dir <- 'out'

# ---

# Define rotating function.
# Method from: https://r-spatial.github.io/sf/articles/sf3.html
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# Load study area shapefile:
sa_sf <- st_read(sa_dir)

# Get study area geometry:
sa_geom <- st_geometry(sa_sf)
# Get study area bounding box:
sa_bb <- st_bbox(sa_geom)
# Get min and max x and y values:
sa_xmin <- sa_bb['xmin']
sa_xmax <- sa_bb['xmax']
sa_ymin <- sa_bb['ymin']
sa_ymax <- sa_bb['ymax']

# Load protected areas shapefile:
pa_sf <- st_read(pa_dir)

# Get count of protected areas:
pa_count <- dim(pa_sf)[1]
message('* ', pa_count, ' protected areas found in shapefile')

# Get protected areas CRS:
pa_crs <- st_crs(pa_sf)
# Get protected areas geometries:
pa_geoms <- st_geometry(pa_sf)

# For each iteration:
for (n_iter in seq_len(n_iters)) {
  # Display a message:
  message(' * Starting iteration ', n_iter, ' of ', n_iters,
          ' (', Sys.time(), ')')
  # Output file path for this iteration:
  out_file <- paste('pas_moved_', n_iter, '.shp', sep='')
  out_path <- paste(out_dir, out_file, sep=fs)
  # If output file exists, move on:
  if (file.exists(out_path)) {
    message('* Output file ', out_path, ' exists. Moving on.')
    next
  }
  # Set random seed value:
  set.seed(42 * n_iter)
  # For each protected area:
  for (pa_index in seq_len(pa_count)) {
    # Display a message:
    message('* Moving and rotating pa ', pa_index, ' of ', pa_count)
    # Get the geometry for this area:
    pa_geom <- pa_geoms[pa_index]
    # Init done variable as FALSE. When this is true, this area has been
    # successfully moved and rotated:
    done <- FALSE
    # Until moved and rotated ... :
    while (done == FALSE) {
      # Pick a new x and y value:
      x_value <- runif(1, sa_xmin, sa_xmax)
      y_value <- runif(1, sa_ymin, sa_ymax)
      # Create a new centroid for this area, and set CRS:
      new_cent <- st_geometry(
        st_point(c(x_value, y_value), dim='XY')
      )
      new_cent <- st_set_crs(new_cent, pa_crs)
      # Check new centroid is in study area, else try again:
      if (length(st_contains(sa_geom, new_cent)[[1]]) != 1) {
        next
      }
      # Pick a rotation value:
      rot_value <- runif(1, 0, 10)
      # Rotate the area, move and set CRS:
      new_poly <- (pa_geom - new_cent) * rot(pi * rot_value) * 1 + new_cent
      new_poly <- st_set_crs(new_poly, pa_crs)
      # Check moved and rotated area is in study area, else try again:
      if (length(st_contains(sa_geom, new_poly)[[1]]) != 1) {
        next
      }
      # Presume new area is o.k. ... :
      new_poly_ok <- 1
      # ... but check it does not overlap any of the other new areas:
      if (pa_index > 1) {
        for (check_poly in new_polys) {
          if (length(st_intersects(check_poly, new_poly)[[1]]) != 0) {
            new_poly_ok <- 0
            break
          }
        }
      }
      # If new area overlaps with any other of the new areas, try again:
      if (new_poly_ok != 1) {
        next
      }
      # If we get here, new area is o.k.:
      done <- TRUE
    }
    # Add the new area to the list:
    if (pa_index > 1) {
      new_polys <- append(new_polys, new_poly)
    } else {
      new_polys <- new_poly
    }
  }
  # Write the new areas to shapefile:
  st_write(new_polys, out_path)
}
