ROUTER_UI_ID <- '_router_ui'
INPUT_BINDING_ID <- '_shiny_router_path'

.onLoad <- function(libname, pkgname) {
  # Adds inst/www directory for loading static resources from server.
  # We need to add that to get all javascript code from client app.
  shiny::addResourcePath('shiny.router.page', system.file('www/bower_components/page/', package = 'shiny.router', mustWork = TRUE))
  shiny::addResourcePath('shiny.router', system.file('www', package = 'shiny.router', mustWork = TRUE))
}

#' Internal function that escapes routing path from not safe characters.
#'
#' @param path A path.
#' @return String with escaped characters.
escape_path <- function(path) {
  clean_path <- gsub("'", "%27", path, fixed = T)
  clean_path <- gsub("\\", "%5C", path, fixed = T)
  clean_path
}

#' Internal function that validates that path is defined in routes.
#'
#' @param routes A routes (list).
#' @param path A path.
#' @return Boolean value indicating if path is defined.
valid_path <- function(routes, path) {
  (!is.null(path) && path %in% names(routes))
}

#' Create single route configuration.
#'
#' @param path Website route.
#' @param ui Valid Shiny user interface.
#' @return A route configuration.
#' @examples
#' route("/", shiny::tags$div(shiny::tags$span("Hello world")))
#'
#' route("/main", div(h1("Main page"), p("Lorem ipsum.")))
#' @export
route <- function(path, ui) {
  out <- list()
  out[[path]] <- ui
  out
}

#' Internal function creating a router callback function.
#' One need to call router callback with Shiny input and output in server code.
#'
#' @param root Main route to which all invalid routes should redirect.
#' @param routes A routes (list).
#' @return Router callback.
create_router_callback <- function(root, routes) {
  function(input, output, session = shiny::getDefaultReactiveDomain()) {

    # A flag to help us display the starting URL's page, instead of going straight
    # to root.
    router_startup <- reactiveVal(TRUE)

    # The page we're currently displaying
    curpage <- reactiveValues(
      path = "/",
      path_and_query = "/"
    )

    output[[ROUTER_UI_ID]] <- shiny::renderUI({
      # initialize_router()
      if (router_startup()) {
        starting_hash <- shiny::getDefaultReactiveDomain()$clientData$url_hash
        if (nzchar(starting_hash)) {
          # TODO: trim shebang from front of starting_hash
          starting_fullpath <- substr(starting_hash, start = 2, stop = nchar(starting_hash))
          location <- httr::parse_url(starting_fullpath)$path
        } else {
          location <- root
          starting_fullpath <- location
        }
        isolate({
          router_startup(FALSE)
          curpage$path_and_query <- starting_fullpath
          curpage$path <- location
        })
      } else {
        location <- get_page()
      }
      if (valid_path(routes, location)) {
        isolate({
          curpage$page <- get_page()
          curpage$page_and_query <- get_path_and_query()
        })
        routes[[location]]
      } else {
        # Ignore invalid routes, and tell page.js to change us back to the
        # current page
        change_page(curpage$path_and_query, session = session)
        routes[[curpage$path]]
      }
    })
  }
}

#' Creates router. Returned callback needs to be called within Shiny server code.
#'
#' @param default Main route to which all invalid routes should redirect.
#' @param ... All other routes defined with shiny.router::route function.
#' @return Shiny router callback that should be run in server code with Shiny input and output lists.
#' @examples
#' router <- make_router(
#'   route("/", root_page),
#'   route("/other", other_page)
#' )
#' @export
make_router <- function(default, ...) {
  routes <- c(default, ...)
  root <- names(default)[1]
  create_router_callback(root, routes)
}

#' Creates an output for router. This configures client side.
#' Call it in your UI Shiny code. In this output ui is going to be rendered
#' according to current routing.
#'
#' @return Shiny tags that configure router and build reactive but hidden input _location.
#' @examples
#' ui <- shinyUI(fluidPage(
#'   router_ui()
#' ))
#' @export
router_ui <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "shiny.router.page/page.js"),
        shiny::tags$script(src = "shiny.router/shiny.router.js")
      )
    ),
    shiny::uiOutput(ROUTER_UI_ID)
  )
}

get_router_field<- function(fieldName, session = shiny::getDefaultReactiveDomain()) {
  if (shiny::isTruthy(session$input[[INPUT_BINDING_ID]])) {
    return(session$input[[INPUT_BINDING_ID]][[fieldName]] )
  } else {
    return(FALSE)
  }
}

#' @export
get_page <- function(session = shiny::getDefaultReactiveDomain()) {
  get_router_field("path", session)
}

#' @export
get_path_and_query <- function(session = shiny::getDefaultReactiveDomain()) {
  get_router_field("path_and_query", session)
}

#' @export
get_query <- function(field = NULL, field_required = TRUE, session = shiny::getDefaultReactiveDomain()) {
  n <- get_router_field("path_and_query", session)
  if(n) {
    if (missing(field)) {
      return(
        httr::parse_url(n)$query
      )
    } else {
      field <- httr::parse_url(n)$query$field
      if (field.required) {
        req(field, cancelOutput = TRUE)
      }
      return(field)
    }
  } else {
    return(FALSE)
  }
}

#' @export
change_page <- function(page, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(page, INPUT_BINDING_ID)
}

#' @export
req_page <- function(page, session = shiny::getDefaultReactiveDomain()) {
  req(
    (get_page(session) == page)
  )
}