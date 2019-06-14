library(shinydashboard)

# MODULE HELPER =========

#' Dashboard Item Wrapper
#'
#' @export
dashboard_item <- function(label, icon, ...){
  id = gsub(' ', '_', label)
  list(
    tab_item=tabItem(tabName = id, ...),
    menu_item=menuItem(label, tabName = id, icon = icon)
  )
}

# MODULE UI ============


#' Wrapper for dashboard layout
#'
#' Wrapper function for producing a dashboard layout. Each argument beyond the title
#' represents one dashboard tab. Use the dashboard_item function to prodcue each
#' tab.
#'
#' @param id shiny id
#' @param input internal
#' @param ... list of dashboard items. Use the dashboard_item function in order to create these items.
#'
#' @rdname mod_dashboard_layout
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody dashboardPage
dashboard_layout <- function(title, head=NULL, skin='blue',...){
  dash_items <- list(...)

  tab_items <- sapply(1:length(dash_items), function(i)dash_items[[i]]$tab_item, simplify = F)
  menu_items <- sapply(dash_items, function(x)x$menu_item, simplify = F)

  HEADER <- dashboardHeader(title = title)
  SIDEBAR <- dashboardSidebar(do.call(sidebarMenu, menu_items))
  BODY <- dashboardBody(tags$head(head), do.call(tabItems, tab_items))

  dashboardPage(HEADER, SIDEBAR, BODY, skin=skin)
}
