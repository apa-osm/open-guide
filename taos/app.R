# libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(glue)
    library(dplyr)
    library(tidyr)
    library(DT)
})

# setup ----


# functions ----
source("scripts/func.R") # helper functions

# user interface ----

## taos_tab ----
taos_tab <- tabItem(tabName = "taos_tab",
    column(
        width = 6,
        ### rr: Registered Report ----
        box(
            title = "Registered Report",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            materialSwitch("is_rr", "Is this a registered report?",
                           FALSE, "success", TRUE),
            tags$div(
                id = "rr_viz",
                class = "viz",
                textInput("rr_link", NULL, placeholder = "Links (DOI or URL) separated by ;")
            )
        ),
        ### pr: Preregistration ----
        box(
            title = "Preregistration",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            materialSwitch("is_pr", "Is there a preregistration?",
                           FALSE, "success", TRUE),
            tags$div(
                id = "pr_viz",
                class = "viz",
                textInput("pr_link", NULL, placeholder = "Links (DOI or URL) separated by ;")
            )
        ),
        ### om: Open Materials ----
        box(
            title = "Open Materials",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            materialSwitch(
                "is_om",
                "Does this paper use any materials that would be needed to reproduce the study?",
                FALSE,
                "success",
                TRUE
            ),
            tags$div(
                id = "om_viz",
                class = "viz",
                materialSwitch(
                    "om_pub",
                    "Are the materials original (i.e., not previously published)?",
                    FALSE,
                    "success",
                    TRUE
                ),
                materialSwitch(
                    "om_share",
                    "Are you able to share the original materials?",
                    TRUE,
                    "success",
                    TRUE
                ),
                textInput("om_noshare", "For what reasons are you unable to share materials?"),
                textInput("om_link", NULL, placeholder = "Links (DOI or URL) separated by ;")
            )
        ),
        ### od: Open Data ----
        box(
            title = "Open Data",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            materialSwitch(
                "is_od",
                "Does this paper use any data (original or archival)?",
                FALSE,
                "success",
                TRUE
            ),
            tags$div(
                id = "od_viz",
                class = "viz",
                materialSwitch(
                    "od_pub",
                    "Is the data original (i.e., not previously published)",
                    FALSE,
                    "success",
                    TRUE
                ),
                materialSwitch(
                    "od_share",
                    "Are you able to share the original data?",
                    TRUE,
                    "success",
                    TRUE
                ),
                textInput("od_noshare", "For what reasons are you unable to share data?"),
                textInput("od_link", NULL, placeholder = "Links (DOI or URL) separated by ;")
            )
        ),
        ### oc: Open Code ----
        box(
            title = "Open Code",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            materialSwitch(
                "is_oc",
                "Does this paper use any code to process or analyse data? This can include R code, SPSS syntax files, or any other way to systematically reproduce the analyses.",
                FALSE,
                "success",
                TRUE
            ),
            tags$div(
                id = "oc_viz",
                class = "viz",
                materialSwitch(
                    "oc_pub",
                    "Is the code original (i.e., not previously published)",
                    FALSE,
                    "success",
                    TRUE
                ),
                materialSwitch(
                    "oc_share",
                    "Are you able to share the original Code?",
                    TRUE,
                    "success",
                    TRUE
                ),
                textInput("oc_noshare", "For what reasons are you unable to share code?"),
                textInput("oc_link", NULL, placeholder = "Links (DOI or URL) separated by ;")
            )
        ),
        ### pr: Preprint ----
        box(
            title = "Preprint",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            p("Preprints are author copies of manuscripts prior to acceptance by a journal. Postprints are author copies of manuscripts after acceptance for publication by a journal. Sharing preprints and postprints increases access to research. Papers are indexed by Google Scholar and Europe PMC, increasing discoverability. Preprint authors also have the chance to receive feedback on their work, improving its quality prior to publication."),
            materialSwitch("is_pp", "Is there a preprint?",
                           FALSE, "success", TRUE),
            tags$div(
                id = "pp_viz",
                class = "viz",
                textInput("pp_link", NULL, placeholder = "Links (DOI or URL) separated by ;")
            )
        )
    ),
    column(
        width = 6,
        ### statement ----
        box(title = "Transparency and Openness Statement", solidHeader = TRUE, 
            collapsible = TRUE, collapsed = FALSE, width = 12,
            verbatimTextOutput("statement")
        )
    )
)

## author_tab ----
author_tab <- tabItem(tabName = "author_tab",
   ### au: Authorship ----
   p("Per the APA Equity, Diversity, and Inclusion Toolkit for editors, “Developed by the Consortia Advancing Standards in Research Administration (CASRAI) and National Information Standards Organization (NISO), CRediT is a high-level taxonomy comprised of 14 contributor roles, each of which can be modified by a degree of contribution (lead, equal, or supporting). The full taxonomy with definitions for each role can be found below. Author contribution statements promote equity and inclusion by specifically attributing contributions to each author. Author contribution statements likewise allow for the inclusion of collaborators whose contributions are less obviously represented in the written article, such as colleagues who may have provided input in early stages of an article or who handled more logistical aspects of the research. CRediT is likewise a useful tool for measuring potential inequities in the division of scientific labor by gender or other demographics."),
   box(
       title = "Authorship",
       width = 12,
       solidHeader = TRUE,
       collapsible = TRUE,
       hidden(numericInput("author_n", NULL, value = 1, min = 1, step = 1)),
       
       fluidRow(
           column(width = 6, textInput("given", NULL, placeholder = "Given Name(s)")),
           column(width = 6, textInput("family", NULL, placeholder = "Family Name(s)"))
       ),
       fluidRow(
           column(width = 8, textInput("orcid", NULL, placeholder = "ORCiD")),
           column(width = 4, actionButton("get_orcid", "Look up ORCiD", icon("orcid"))),
           column(width = 12, uiOutput("orcid_chooser"))
       ),
       pickerInput("roles", "Roles",
                   choices = credit_roles("names"),
                   multiple = TRUE),
       # purrr::map2(credit_roles("abbr"), credit_roles("names"), ~{
       #     tags$tr(id = paste0(.x, "_row"),
       #         tags$td(.y),
       #         tags$td(radioGroupButtons(.x, NULL, 
       #                 choices = c("lead", "equal", "supporting", "none"), 
       #                 selected = "none", size = "xs"))
       #     )
       # }) %>% do.call(tags$table, .),
       actionButton("add_author", "Add Author", icon = icon("plus"))
   ),
### author_list ----
box(title = "Authorship Table", solidHeader = TRUE, 
    collapsible = TRUE, collapsed = FALSE, width = 12,
    DTOutput("author_list"),
    downloadButton("author_table_download")
),
### credit_roles ----
box(title = "CRediT Roles", solidHeader = TRUE, 
    collapsible = TRUE, collapsed = TRUE, width = 12,
    uiOutput("credit_roles")
),
### credit_jats ----
box(title = "CRediT Taxonomy JATS Format", solidHeader = TRUE, 
    collapsible = TRUE, collapsed = TRUE, width = 12,
    verbatimTextOutput("credit_jats")
)
)


## UI ----
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Transparency and Openness",
                    titleWidth = "calc(100% - 44px)"),
    dashboardSidebar(
        sidebarMenu(id = "tabs",
        menuItem("Statement", tabName = "taos_tab",
                 icon = icon("yin-yang")),
        menuItem("Authors", tabName = "author_tab",
                 icon = icon("user-graduate"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src = "custom.js")
        ),
        
        tabItems(
            taos_tab,
            author_tab
        )
    )
)


# server ----
server <- function(input, output, session) {
    hide("orcid_chooser")
    
    # reactive values ----
    authors <- reactiveVal(list())
    
    # reactives ----
    
    ## author_table ----
    author_table <- reactive({
        req(length(authors()) > 0)
        
        role_trans <- setNames(credit_roles("abbr"),
                               credit_roles("names"))
        
        authors <- do.call(bind_rows, authors())
        
        if ("roles" %in% names(authors)) {
            authors <- authors %>%
                mutate(x = "x"
                       #roles = recode(roles, !!!role_trans)
                       ) %>%
                pivot_wider(names_from = roles,
                            values_from = x,
                            values_fill = "")
        }
        
        authors %>%
            mutate(order = row_number()) %>%
            select(order, everything())
    })
    
    
    
    # observers ----
    
    ## toggle viz ----
    observe({ toggle("rr_viz", condition = input$is_rr) })
    observe({ toggle("pr_viz", condition = input$is_pr) })
    observe({ toggle("om_viz", condition = input$is_om) })
    observe({toggle("od_viz", condition = input$is_od) })
    observe({ toggle("oc_viz", condition = input$is_oc) })
    observe({ toggle("pp_viz", condition = input$is_pp) })
    
    ## share/noshare ----
    observe({ toggle("om_share", condition = input$om_pub) })
    observe({ toggle("od_share", condition = input$od_pub) })
    observe({ toggle("oc_share", condition = input$oc_pub) })
    
    observe({ toggle("om_noshare", condition = !input$om_share & input$om_pub) })
    observe({ toggle("od_noshare", condition = !input$od_share & input$od_pub) })
    observe({ toggle("oc_noshare", condition = !input$oc_share & input$oc_pub) })
    
    observe({ toggle("om_link", condition = input$om_share | !input$om_pub) })
    observe({ toggle("od_link", condition = input$od_share | !input$od_pub) })
    observe({ toggle("oc_link", condition = input$oc_share | !input$oc_pub) })
    
    ## get_orcid ----
    observeEvent(input$get_orcid, {
        debug_msg("-- get_orcid")
        
        o <- tryCatch({
            get_orcid(input$family, input$given)
        }, error = function(e) {
            return(NULL)
        })
        
        n <- length(o)
        if (n == 1) {
            updateTextInput(session, "orcid", value = o)
        } else if (n == 0) {
            sprintf("%d ORCiDs found", n) %>%
                shinyjs::alert()
        } else {
            output$orcid_chooser <- renderUI({
                lapply(o, function(x) {
                    glue("  <li>{x$surname}, {x$given}: <a target='_blank' href='https://orcid.org/{x$orcid}'>{x$orcid}</a></li>")
                }) %>%
                    paste(collapse = "\n") %>%
                    paste0("<ul>", ., "</ul>") %>%
                    HTML()
            })
            show("orcid_chooser")
        }
    })
    
    ## add_author ----
    observeEvent(input$add_author, {
        debug_msg("-- add_author")
        aa <- authors()
        au <- list(
                    given = input$given,
                    family = input$family,
                    orcid = input$orcid,
                    roles = input$roles
            )
        aa[[input$author_n]] <- au
        authors(aa)
        
        updateNumericInput(session, "author_n", value = length(aa) + 1)
        updateActionButton(session, "add_author", "Add Author")
        updateTextInput(session, "given", value = "")
        updateTextInput(session, "family", value = "")
        updateTextInput(session, "orcid", value = "")
        updatePickerInput(session, "roles", selected = character())
        hide("orcid_chooser")
    })
    
    ## edit_author ----
    observeEvent(input$author_list_rows_selected, {
        debug_msg("edit_author")
        idx <- input$author_list_rows_selected
        
        a <- authors()[[idx]]
        updateNumericInput(session, "author_n", value = idx)
        updateTextInput(session, "given", value = a$given)
        updateTextInput(session, "family", value = a$family)
        updateTextInput(session, "orcid",
                        value = ifelse(isFALSE(a$orcid), "", a$orcid))
        updatePickerInput(session, "roles", selected = a$roles)
        updateActionButton(session, "add_author", "Update Author")
        
        shinyjs::removeClass("given", "warning")
        shinyjs::removeClass("surname", "warning")
        shinyjs::removeClass("orcid", "warning")
        shinyjs::show("aut_delete")
    }, ignoreNULL = TRUE)
    
    # outputs ----
    ## statement ----
    output$statement <- renderText({
        stmts <- list(
            "Registered Report: {rr}",
            "Preregistration: {pr}",
            "Open Materials: {om} {om_pub} {om_share}",
            "Open Data: {od} {od_pub} {od_share}",
            "Open Code: {oc} {oc_pub} {oc_share}",
            "Preprint: {pp}"
        )
        
        om_pub = case_when(
            !input$is_om ~ "",
            !input$om_pub ~ "(Previously Published)",
            TRUE ~ "(Original)"
        )
        od_pub = case_when(
            !input$is_od ~ "",
            !input$od_pub ~ "(Previously Published)",
            TRUE ~ "(Original)"
        )
        oc_pub = case_when(
            !input$is_oc ~ "",
            !input$oc_pub ~ "(Previously Published)",
            TRUE ~ "(Original)"
        )
        
        om_share = ifelse(!input$om_share, input$om_noshare, "")
        od_share = ifelse(!input$od_share, input$od_noshare, "")
        oc_share = ifelse(!input$oc_share, input$oc_noshare, "")
        
        rr = ifelse(input$is_rr, input$rr_link, "No")
        pr = ifelse(input$is_pr, input$pr_link, "No")
        om = ifelse(input$is_om, input$om_link, "N/A")
        od = ifelse(input$is_od, input$od_link, "N/A")
        oc = ifelse(input$is_oc, input$oc_link, "N/A")
        pp = ifelse(input$is_pp, input$pp_link, "No")
        
        txt <- glue(paste(stmts, collapse = "\n"))
        gsub(" +", " ", txt)
    })
    
    ## author_list ----
    output$author_list <- renderDT({ 
        author_table()
    }, escape = F,
        selection = "single",
        extensions = "RowReorder",
        rownames = FALSE,
        options = list(
            info = FALSE,
            lengthChange = FALSE,
            paging = FALSE,
            ordering = FALSE,
            searching = FALSE,
            pageLength = 500,
            keys = TRUE,
            rowReorder = TRUE,
            order = list(c(0 , 'asc'))
        ),
        callback = JS(c(
            "table.on('row-reorder', function(e, details, changes) {",
            "  var op = JSON.stringify(details);",
            "  Shiny.onInputChange('aut_reorder', op);",
            "});")))
    
    ## credit taxonomy ----
    output$credit_roles <- renderUI({
        x <- utils::capture.output(credit_roles())
        x %>%
            gsub("^\\[\\S+\\] ", "* **", .) %>%
            gsub(": ", "**: ", .) %>%
            paste(collapse = "\n") %>%
            markdown::renderMarkdown(text = .) %>%
            HTML()
    })
    
    ## credit_jats ----
    output$credit_jats <- renderText({
        lapply(authors(), author_jats) %>%
            paste(collapse = "\n") %>%
            paste0("<contrib-group>\n", ., "\n</contrib-group>")
    })
    
    ## author_table_download ----
    output$author_table_download <- downloadHandler(
        filename = function() {
            "authors.csv"
        },
        content = function(file) {
            readr::write_csv(author_table(), file)
        }
    )
    
}

shinyApp(ui, server)
