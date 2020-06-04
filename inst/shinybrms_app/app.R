library(shiny)

####################################################################################################
# UI
####################################################################################################

ui <- navbarPage(
  "shinybrms", id = "navbar_ID",
  tabPanel(
    "Data",
    titlePanel("Data"),
    br(),
    sidebarLayout(
      sidebarPanel(
        helpText("Either choose an example dataset or upload a file",
                 "(*.csv, *.txt, or *.dat) containing your own dataset.",
                 "In either case, a preview of the dataset will be shown in the main panel on the right.",
                 "If you want to upload a dataset after having chosen an example dataset, you have",
                 "to clear out the name of the example dataset from the input field",
                 "\"Choose example dataset ...\"."),
        helpText(HTML(paste0("The following data entries are recognized as missing values: empty ",
                             "(i.e. nothing, not even a whitespace), whitespace, ", code("NA"),
                             ", ", code("."), " (dot)."))),
        # Horizontal line (first one, so set the global hr() style here):
        hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
        h4("Example dataset"),
        selectInput("ex_da_sel", NULL,
                    choices = c("Choose example dataset ..." = "",
                                "Arabidopsis (from package \"lme4\")" = "Arabidopsis",
                                "bacteria (from package \"MASS\")" = "bacteria",
                                "birthwt (from package \"MASS\")" = "birthwt",
                                "epilepsy (from package \"brms\")" = "epilepsy",
                                "grouseticks (from package \"lme4\")" = "grouseticks",
                                "kidiq (from package \"rstanarm\")" = "kidiq",
                                "Puromycin" = "Puromycin",
                                "quine (from package \"MASS\")" = "quine",
                                "Rabbit (from package \"MASS\")" = "Rabbit",
                                "roaches (from package \"rstanarm\")" = "roaches",
                                "sleepstudy (from package \"lme4\")" = "sleepstudy",
                                "ToothGrowth" = "ToothGrowth"),
                    selectize = TRUE),
        hr(),
        h4("Upload a dataset"),
        fileInput("file_upload", "Choose file:",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values",
                             "text/plain",
                             ".csv",
                             ".txt",
                             ".dat")),
        strong("Header"),
        checkboxInput("header", "File has header", TRUE),
        radioButtons("sep", "Separator",
                     choices = c("Comma" = ",",
                                 "Semicolon" = ";",
                                 "Tab" = "\t",
                                 "Whitespace" = "")),
        radioButtons("quote", "Quote",
                     choices = c("None" = "",
                                 "Double quote" = '"',
                                 "Single quote" = "'"),
                     selected = '"'),
        radioButtons("dec", "Decimal",
                     choices = c("Point" = ".",
                                 "Comma" = ",")),
        hr(),
        h4("Preview"),
        radioButtons("preview_type_radio", "Type of preview",
                     choices = c("Dataset" = "datas",
                                 "Structure" = "struc")),
        radioButtons("preview_rows_radio", "Rows to show (only for preview type \"Dataset\")",
                     choices = c("Head (only the first 6 rows)" = "head",
                                 "All rows" = "all"))
        
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.preview_type_radio == 'datas'",
          tableOutput("da_view")
        ),
        conditionalPanel(
          condition = "input.preview_type_radio == 'struc'",
          verbatimTextOutput("da_str", placeholder = FALSE)
        )
      )
    )
  ),
  tabPanel(
    "Likelihood",
    titlePanel("Likelihood"),
    br(),
    navlistPanel(
      tabPanel(
        "Outcome",
        titlePanel("Outcome"),
        br(),
        helpText("Choose the outcome (the dependent variable) and the distributional family for this outcome."),
        selectInput("outc_sel", "Outcome:",
                    choices = c("Choose outcome ..." = ""),
                    selectize = TRUE),
        selectInput("dist_sel", "Distributional family for the outcome:",
                    choices = list(
                      "Choose distributional family for the outcome ..." =
                        c("Choose distributional family for the outcome ..." = ""),
                      "Continuous outcome:" =
                        c("Gaussian (normal)" = "gaussian"),
                      "Binary outcome:" =
                        c("Bernoulli with logit link" = "bernoulli"),
                      "Count data outcome:" =
                        c("Negative binomial" = "negbinomial")
                    ),
                    selectize = TRUE),
        strong("Parameters (with corresponding link functions) specific to this distributional family:"),
        tableOutput("dist_link"),
        helpText(HTML(paste0(
          "For details concerning the link functions, see the help for function ",
          code("brms::brmsfamily()"),
          " from package ",
          a(HTML("<strong>brms</strong>"),
            href = "https://CRAN.R-project.org/package=brms",
            target = "_blank"),
          " and the ", strong("brms"), " vignette ",
          a("\"Parameterization of Response Distributions in brms\"",
            href = "https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html",
            target = "_blank"),
          ". Note that for each parameter, the link function only applies if this parameter is ",
          "actually modeled by (non-constant) predictors. Currently, this is only supported ",
          "for the location parameter (e.g. ", code("mu"), " for a Gaussian distribution)."
        )))
      ),
      tabPanel(
        "Predictors",
        titlePanel("Predictors"),
        br(),
        helpText("Choose the predictors (the independent variables). More specifically, you may define",
                 "main effects of predictors and interactions between predictors.",
                 "An intercept (i.e. a constant) will always be included.",
                 "Numeric variables (with \"numeric\" including \"integer\") are treated as continuous",
                 "predictors. Non-numeric variables are treated as nominal predictors. The type of",
                 "a variable may be seen on the \"Data\" page when choosing the \"Structure\" preview",
                 "type. If you want a numeric variable to be treated as a nominal predictor, you have",
                 "to convert this variable in your dataset to a character variable, e.g. by",
                 "changing the value \"1\" to \"level1\", the value \"2\" to \"level2\" and so on.",
                 "For nominal predictors, the first level (after sorting alphabetically) will be the",
                 "reference level."),
        wellPanel(
          h3("Main effects"),
          helpText("Note:",
                   tags$ul(
                     tags$li("Non-varying effects are also known as population-level or \"fixed\" effects."),
                     tags$li("Varying effects are also known as group-level or \"random\" effects."),
                   ),
                   "The terms \"fixed\" and \"random\" effects are put in quotation marks as they",
                   "are not really appropriate for a Bayesian regression model."),
          h4("Non-varying main effects"),
          helpText("Start typing or click into the field below to choose variables for which",
                   "nonvarying main effects shall be added."),
          selectInput("pred_mainNV_sel", NULL,
                      choices = c("Choose variables for nonvarying main effects ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE),
          h4("Varying intercepts"),
          helpText("Start typing or click into the field below to choose variables for which",
                   "varying intercepts shall be added."),
          selectInput("pred_mainV_sel", NULL,
                      choices = c("Choose variables for varying intercepts ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE)
        ),
        wellPanel(
          h3("Interaction effects"),
          helpText("Only variables already having a main effect may be included in an interaction",
                   "term. In the rare case that you really need an interaction involving a variable",
                   "without a main effect, you have to include this interaction manually as a",
                   "variable in your dataset and add a main effect for this manually created",
                   "interaction variable.",
                   "Start typing or click into the field below to choose variables for which an",
                   "interaction term shall be added. Confirm this interaction term by pressing the",
                   "\"Add interaction term\" button. All interaction terms which have been added are",
                   "listed in the box below the \"Add interaction term\" button. You may edit this list",
                   "to remove individual interaction terms or to re-add interaction terms which you",
                   "have previously removed."),
          selectInput("pred_int_build", NULL,
                      choices = c("Choose variables for an interaction term ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE),
          actionButton("pred_int_add", "Add interaction term"),
          br(),
          br(),
          selectInput("pred_int_sel", "Added interaction terms (you may edit this list, see above):",
                      choices = NULL,
                      multiple = TRUE,
                      selectize = TRUE)
        ),
        wellPanel(
          h3("Preview of chosen predictor terms"),
          helpText(HTML(paste0(
            "Here, you can get a preview of the currently chosen predictor terms. ",
            "This is mainly intended as a check for those familiar with R's and ",
            a(HTML("<strong>brms</strong>"),
              href = "https://CRAN.R-project.org/package=brms",
              target = "_blank"),
            "'s formula syntax. A preview of the full formula is given in the tab \"Formula ",
            "preview\" which may be found in the panel on the left-hand side."
          ))),
          tableOutput("pred_view")
        )
      ),
      tabPanel(
        "Formula preview",
        titlePanel("Formula preview"),
        br(),
        strong("Current formula:"),
        verbatimTextOutput("formula_view", placeholder = TRUE)
      )
    )
  ),
  tabPanel(
    "Prior",
    titlePanel("Prior"),
    br(),
    helpText(HTML(paste0(
      "Build the joint prior distribution of all parameters in your model by ",
      "placing independent priors on all parameters separately. ",
      "For parameters for which you do not specify a custom prior, the default prior from the ",
      "package ", strong("brms"), " will be used. ",
      "Note that the parameter \"Intercept\" is the intercept when centering the predictors ",
      "(this is only the internally used intercept; for the output, the intercept with ",
      "respect to the non-centered predictors is given (named \"b_Intercept\")). ",
      "For details concerning the prior, see the help for functions ",
      code("brms::get_prior()"),
      " and ",
      code("brms::set_prior()"),
      " from package ",
      a(HTML("<strong>brms</strong>"),
        href = "https://CRAN.R-project.org/package=brms",
        target = "_blank"),
      " as well as the ",
      a(HTML("Stan documentation"),
        href = "https://mc-stan.org/users/documentation/",
        target = "_blank"),
      " (in particular, the \"Stan Functions Reference\")."
    ))),
    hr(),
    h3("Default priors"),
    br(),
    strong("Default priors for the parameters belonging to the current likelihood:"),
    tableOutput("prior_default_view"),
    helpText("An empty field in column \"Prior\" denotes a flat prior over the domain of the",
             "corresponding parameter."),
    hr(),
    h3("Custom priors"),
    br(),
    sidebarLayout(
      sidebarPanel(
        h4("Specification of custom priors"),
        br(),
        selectInput("prior_class_sel", "Parameter class (may consist of a single parameter):",
                    choices = c("Choose parameter class ..." = ""),
                    selectize = TRUE),
        selectInput("prior_coef_sel", paste("Coefficient (leave empty to use all coefficients",
                                            "belonging to the selected parameter class):"),
                    choices = c("Choose coefficient or leave empty" = ""),
                    selectize = TRUE),
        selectInput("prior_group_sel", paste("Group (for varying effects) (if no coefficient is",
                                             "selected: leave empty to use all groups belonging to",
                                             "the selected parameter class; if you are not able to",
                                             "clear the field while having no coefficient selected,",
                                             "then first clear the \"Parameter class\" field):"),
                    choices = character(),
                    selectize = TRUE),
        textInput("prior_text", "Prior distribution (in Stan language or leave empty to use a flat prior):",
                  value = "",
                  placeholder = "Enter prior distribution in Stan language or leave empty to use a flat prior"),
        actionButton("prior_add", "Add prior"),
        br(),
        br(),
        br(),
        actionButton("prior_reset", "Reset all custom priors")
      ),
      mainPanel(
        h4("Preview of custom priors"),
        br(),
        strong("Custom priors currently set:"),
        tableOutput("prior_set_view"),
        helpText("An empty field in column \"Prior\" denotes a flat prior over the domain of the",
                 "corresponding parameter.")
      )
    )
  ),
  tabPanel(
    "Posterior",
    titlePanel("Posterior"),
    br(),
    helpText("Use",
             a("Stan", href = "https://mc-stan.org/", target = "_blank"),
             "to infer the joint posterior distribution of all parameters in your model",
             "by sampling."),
    wellPanel(
      h3("Stan code"),
      helpText(
        "Here, you can get a preview of the Stan code for your model and download it.",
        
        "The data used in the", code("data {...}"), "program block of the Stan code is the Stan",
        "data. Thus, the Stan code goes together with the Stan data.",
        
        "Apart from checking purposes,",
        "this is useful for example if you want to customize the model and then run Stan by yourself."
      ),
      checkboxInput("show_stancode", "Show Stan code", value = FALSE),
      conditionalPanel(
        condition = "input.show_stancode",
        verbatimTextOutput("stancode_view", placeholder = TRUE),
      ),
      downloadButton("stancode_download", "Download Stan code")
    ),
    wellPanel(
      h3("Stan data"),
      helpText(
        "Here, you can get a preview of the structure of the Stan data for your model and download it.",
        
        "The Stan data is the data used in the", code("data {...}"), "program block in the Stan",
        "code. Thus, the Stan data goes together with the Stan code.",
        
        "Apart from checking purposes,",
        "this is useful for example if you want to customize the model and then run Stan by yourself."
      ),
      checkboxInput("show_standata", "Show structure of Stan data", value = FALSE),
      conditionalPanel(
        condition = "input.show_standata",
        verbatimTextOutput("standata_view", placeholder = TRUE),
      ),
      downloadButton("standata_download", "Download Stan data")
    ),
    wellPanel(
      h3("Advanced options"),
      helpText(HTML(paste0(
        "Here, you can set advanced options for the function ",
        code("brms::brm()"),
        "which is the central function for inferring the posterior. ",
        "These advanced options have sensible defaults, but sometimes they need to be changed. ",
        "For details on these advanced options, see the help for ",
        code("brms::brm()"),
        " from package ",
        a(HTML("<strong>brms</strong>"),
          href = "https://CRAN.R-project.org/package=brms",
          target = "_blank"),
        " and the help for ",
        code("rstan::sampling()"),
        " as well as for ",
        code("rstan::stan()"),
        " from package ",
        a(HTML("<strong>rstan</strong>"),
          href = "https://CRAN.R-project.org/package=rstan",
          target = "_blank"),
        "."
      ))),
      checkboxInput("show_advOpts", "Show advanced options", value = FALSE),
      conditionalPanel(
        condition = "input.show_advOpts",
        helpText("Notes:",
                 tags$ol(
                   tags$li(paste("To obtain reproducible results, you need to specify a value for",
                                 "option \"Seed\" and enter this value each time you want to",
                                 "obtain the same results. Leave option \"Seed\" empty to use a",
                                 "random seed (giving non-reproducible results).")),
                   tags$li("Numeric options with an empty field (apart from option \"Seed\") have",
                           "a default value which depends on other options. Leave them empty to",
                           "use this default value."),
                   tags$li("Numeric options with a preset value may not be left empty."),
                   tags$li(paste("Internally, the number of cores is set automatically to the",
                                 "minimum value of options \"Cores\" and \"Chains\"."))
                 )),
        fluidRow(
          column(5,
                 numericInput("advOpts_seed", "Seed:",
                              value = NA, step = 1L),
                 numericInput("advOpts_cores", "Cores:",
                              value = getOption("mc.cores", parallel::detectCores()), step = 1L, min = 1L),
                 numericInput("advOpts_chains", "Chains:",
                              value = 4L, step = 1L, min = 1L),
                 numericInput("advOpts_iter", "Total iterations per chain:",
                              value = 2000L, step = 1L, min = 1L),
                 numericInput("advOpts_warmup", "Warmup iterations per chain:",
                              value = NA, step = 1L, min = 0L),
                 numericInput("advOpts_thin", "Thinning rate:",
                              value = 1L, step = 1L, min = 1L)),
          column(5, offset = 1,
                 radioButtons("advOpts_inits", "Initial values:",
                              choices = list("Random" = "random", "Zero" = "0"),
                              inline = TRUE),
                 numericInput("advOpts_init_r", "\"init_r\" (only relevant for random initial values):",
                              value = 2, step = 0.1, min = 0),
                 numericInput("advOpts_adapt_delta", "\"adapt_delta\":",
                              value = 0.95, step = 0.01, min = 0, max = 1),
                 numericInput("advOpts_max_treedepth", "\"max_treedepth\":",
                              value = 15L, step = 1L, min = 1L),
                 checkboxInput("advOpts_open_progress", "Open progress",
                               value = TRUE),
                 numericInput("advOpts_refresh", "Progress-refreshing step size:",
                              value = NA, step = 1L, min = 0L),
                 checkboxInput("advOpts_save_all_pars", "\"save_all_pars\"",
                               value = FALSE),
                 checkboxInput("advOpts_save_warmup", "Save warmup",
                               value = TRUE))
        )
      )
    ),
    wellPanel(
      h3("Run Stan"),
      helpText("Note: If the advanced option \"Open progress\" is selected (as per default),",
               "Windows users having Firefox set as their default web browser may need to manually",
               "copy the link to the Stan HTML progress file which is automatically opening up and",
               "paste this link into a different web browser for viewing the progress file there."),
      actionButton("run_stan", "Run Stan (may take a while)")
    ),
    wellPanel(
      h3("Output"),
      strong("Date and time when Stan run was finished:"),
      textOutput("fit_date"),
      br(),
      strong("Summary:"),
      verbatimTextOutput("smmry_view", placeholder = TRUE),
      selectInput("stanout_download_sel", "Choose output file to download:",
                  choices = c("\"brmsfit\" object (RDS file)" = "brmsfit_obj",
                              "Matrix of posterior draws (CSV file)" = "draws_mat_csv",
                              "Matrix of posterior draws (RDS file)" = "draws_mat_obj",
                              "Array of posterior draws (RDS file)" = "draws_arr_obj"),
                  selectize = TRUE),
      helpText(HTML(paste0("The most comprehensive output object is the \"brmsfit\" object which ",
                           "is the output from ", code("brms::brm()"), ", the central function ",
                           "for inferring the posterior."))),
      downloadButton("stanout_download", "Download output file"),
      br(),
      br(),
      h4("Interactive output inspection using package", strong("shinystan")),
      helpText(
        "Note: In the", strong("shinystan"), "app, the parameter names given by", strong("brms"), "are used.",
        "These are as follows:",
        tags$ul(
          tags$li("\"b_Intercept\" is the intercept (with respect to the non-centered predictors)."),
          tags$li("The parameters starting with \"b_\" are the regression coefficients."),
          tags$li(HTML(paste("All other parameters are parameters specific to the chosen distributional family",
                             "for the outcome (see page \"Likelihood\" &rarr; \"Outcome\").")))
        )
      ),
      numericInput("seed_PPD",
                   paste("Seed for draws from posterior predictive distribution",
                         "(leave empty to use a random seed):"),
                   value = NULL, step = 1L),
      actionButton("act_launch_shinystan", HTML(paste("Launch", strong("shinystan"), "(may take a while)")))
    )
  ),
  navbarMenu(
    "More",
    tabPanel(
      "About",
      titlePanel("About \"shinybrms\""),
      br(),
      wellPanel(
        h3("Basic information"),
        helpText(HTML(paste0(
          "Note: This is the ", strong("shinybrms"), " ", em("app"), ". It is distributed under ",
          "the same name as an ",
          a("R", href = "https://www.R-project.org/", target = "_blank"),
          " package which is available on ",
          a("GitHub", href = "https://github.com/fweber144/shinybrms", target = "_blank"), " and ",
          a("CRAN", href = "https://CRAN.R-project.org/package=shinybrms", target = "_blank"), "."
        ))),
        tags$ul(
          tags$li(HTML(paste0(
            strong("Description: "),
            "The ", strong("shinybrms"), " app is a graphical user interface (GUI) for the R package ",
            a(HTML("<strong>brms</strong>"),
              href = "https://CRAN.R-project.org/package=brms",
              target = "_blank"),
            " which allows to fit Bayesian regression models using ",
            a("Stan", href = "https://mc-stan.org/", target = "_blank"),
            " (more specifically, using its R interface, the package ",
            a(HTML("<strong>rstan</strong>"),
              href = "https://CRAN.R-project.org/package=rstan",
              target = "_blank"),
            "). The GUI is a ",
            a("Shiny", href = "https://shiny.rstudio.com/", target = "_blank"),
            " app, i.e. created using the R package ",
            a(HTML("<strong>shiny</strong>"),
              href = "https://CRAN.R-project.org/package=shiny",
              target = "_blank"),
            "."
          ))),
          tags$li(strong("Author:"),
                  "Frank Weber"),
          tags$li(strong("Version:"),
                  "1.0.1.9000"),
          tags$li(strong("Date (yyyy-mm-dd):"),
                  "2020-03-26"),
          tags$li(strong("License:"),
                  "GPL-3")
        )
      ),
      wellPanel(
        h3("Trademarks"),
        tags$ul(
          tags$li(HTML(paste0(
            "The Stan name is a registered trademark of ",
            a("NumFOCUS under the direction of the Stan Leadership Body",
              href = "https://mc-stan.org/about/numfocus/index.html",
              target = "_blank"),
            "."
          ))),
          tags$li("Firefox is a trademark of the Mozilla Foundation in the U.S. and other countries.")
        )
      )
    ),
    tabPanel(
      "Links",
      titlePanel("Links"),
      br(),
      wellPanel(
        h3("Software (without R packages)"),
        tags$ul(
          tags$li(a("R", href = "https://www.R-project.org/", target = "_blank")),
          tags$li(a("Stan", href = "https://mc-stan.org/", target = "_blank"))
        )
      ),
      wellPanel(
        h3("R packages"),
        tags$ul(
          tags$li(HTML(paste0(
            strong("shinybrms"), ": ",
            a("GitHub", href = "https://github.com/fweber144/shinybrms", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=shinybrms", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("brms"), ": ",
            a("GitHub", href = "https://github.com/paul-buerkner/brms", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=brms", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("rstan"), ": ",
            a("GitHub", href = "https://github.com/stan-dev/rstan/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=rstan", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("shinystan"), ": ",
            a("GitHub", href = "https://github.com/stan-dev/shinystan/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=shinystan", target = "_blank")
          )))
        )
      )
    ),
    tabPanel(
      "Help",
      titlePanel("Help"),
      br(),
      wellPanel(
        h3("Software"),
        HTML(paste0("For help concerning the software used in the ", strong("shinybrms"), " app ",
                    "(including the R package ", strong("shinybrms"), "), see ",
                    "the \"More\" &rarr; \"Links\" page."))
      ),
      wellPanel(
        h3("Literature"),
        "These are helpful textbooks on Bayesian statistics:",
        tags$ul(
          tags$li("Introductory:",
                  tags$ul(
                    tags$li("McElreath, R. (2016).",
                            em("Statistical Rethinking: A Bayesian Course with Examples in R and Stan"),
                            "(1st ed.",
                            em("[Note that there is also a second edition available.]"),
                            "). CRC Press.")
                  )),
          tags$li("Introductory to more advanced:",
                  tags$ul(
                    tags$li("Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2014).",
                            em("Bayesian Data Analysis"),
                            "(3rd ed.). CRC Press.")
                  ))
        )
      )
    )
  ),
  tabPanel(title = "Quit", value = "quit_app", icon = icon("power-off"))
)

####################################################################################################
# Server
####################################################################################################

server <- function(input, output, session){
  
  #-------------------------------------------------
  # Data
  
  da <- reactive({
    if(identical(input$ex_da_sel, "Arabidopsis")){
      if(requireNamespace("lme4", quietly = TRUE)){
        tmp_env <- new.env()
        data(Arabidopsis, package = "lme4", envir = tmp_env)
        assign("Arabidopsis", within(get("Arabidopsis", envir = tmp_env), {
          gen <- as.factor(gen)
          rack <- as.factor(rack)
          nutrient <- as.factor(nutrient)
        }), envir = tmp_env)
        return(get("Arabidopsis", envir = tmp_env))
      } else{
        showNotification(
          "Package \"lme4\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "bacteria")){
      if(requireNamespace("MASS", quietly = TRUE)){
        tmp_env <- new.env()
        data(bacteria, package = "MASS", envir = tmp_env)
        return(get("bacteria", envir = tmp_env))
      } else{
        showNotification(
          "Package \"MASS\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "birthwt")){
      if(requireNamespace("MASS", quietly = TRUE)){
        tmp_env <- new.env()
        data(birthwt, package = "MASS", envir = tmp_env)
        assign("birthwt", within(get("birthwt", envir = tmp_env), {
          # Code from ?MASS::birthwt, but slightly modified:
          low <- as.factor(low)
          race <- factor(race, labels = c("white", "black", "other"))
          smoke <- as.factor(smoke > 0)
          ptl_2cat <- as.factor(ptl > 0)
          ht <- as.factor(ht > 0)
          ui <- as.factor(ui > 0)
          ftv_3cat <- as.factor(ftv)
          levels(ftv_3cat)[-(1:2)] <- "2+"
        }), envir = tmp_env)
        return(get("birthwt", envir = tmp_env))
      } else{
        showNotification(
          "Package \"MASS\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "epilepsy")){
      if(requireNamespace("brms", quietly = TRUE)){
        tmp_env <- new.env()
        data(epilepsy, package = "brms", envir = tmp_env)
        return(get("epilepsy", envir = tmp_env))
      } else{
        showNotification(
          "Package \"brms\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "grouseticks")){
      if(requireNamespace("lme4", quietly = TRUE)){
        tmp_env <- new.env()
        data(grouseticks, package = "lme4", envir = tmp_env)
        return(get("grouseticks", envir = tmp_env))
      } else{
        showNotification(
          "Package \"lme4\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "kidiq")){
      if(requireNamespace("rstanarm", quietly = TRUE)){
        tmp_env <- new.env()
        data(kidiq, package = "rstanarm", envir = tmp_env)
        return(get("kidiq", envir = tmp_env))
      } else{
        showNotification(
          "Package \"rstanarm\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "Puromycin")){
      return(Puromycin)
    } else if(identical(input$ex_da_sel, "quine")){
      if(requireNamespace("MASS", quietly = TRUE)){
        tmp_env <- new.env()
        data(quine, package = "MASS", envir = tmp_env)
        return(get("quine", envir = tmp_env))
      } else{
        showNotification(
          "Package \"MASS\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "Rabbit")){
      if(requireNamespace("MASS", quietly = TRUE)){
        tmp_env <- new.env()
        data(Rabbit, package = "MASS", envir = tmp_env)
        return(get("Rabbit", envir = tmp_env))
      } else{
        showNotification(
          "Package \"MASS\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "roaches")){
      if(requireNamespace("rstanarm", quietly = TRUE)){
        tmp_env <- new.env()
        data(roaches, package = "rstanarm", envir = tmp_env)
        return(get("roaches", envir = tmp_env))
      } else{
        showNotification(
          "Package \"rstanarm\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "sleepstudy")){
      if(requireNamespace("lme4", quietly = TRUE)){
        tmp_env <- new.env()
        data(sleepstudy, package = "lme4", envir = tmp_env)
        return(get("sleepstudy", envir = tmp_env))
      } else{
        showNotification(
          "Package \"lme4\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "ToothGrowth")){
      return(ToothGrowth)
    } else{
      req(input$file_upload)
      tryCatch({
        return(read.csv(input$file_upload$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote,
                        dec = input$dec,
                        na.strings = c("NA", ".")))
      }, error = function(err){
        # Return a safeError if a parsing error occurs:
        stop(safeError(err))
      })
    }
  })
  
  #-------------------------------------------------
  # Data preview
  
  output$da_view <- renderTable({
    if(identical(input$preview_rows_radio, "head")){
      return(head(da()))
    } else{
      return(da())
    }
  })
  
  output$da_str <- renderPrint({
    str(da())
  })
  
  #-------------------------------------------------
  # Outcome
  
  #------------------------
  # Outcome
  
  observe({
    updateSelectInput(session, "outc_sel",
                      choices = c("Choose outcome ..." = "",
                                  setdiff(names(da()),
                                          c(input$pred_mainNV_sel,
                                            input$pred_mainV_sel))),
                      selected = isolate(input$outc_sel))
  })
  
  #------------------------
  # Distributional family
  
  C_family <- reactive({
    req(input$dist_sel)
    brms::brmsfamily(family = input$dist_sel)
  })
  
  dist_link_da <- reactive({
    req(C_family())
    if(identical(input$dist_sel, "")){
      return(
        data.frame("Parameter" = character(),
                   "Link function" = character(),
                   check.names = FALSE)
      )
    } else{
      C_family_list <- C_family()
      dist_link_tmp <- data.frame("Parameter" = C_family_list$dpars,
                                  "Link function" = NA,
                                  check.names = FALSE)
      dist_link_tmp$"Link function" <- sapply(dist_link_tmp$"Parameter", function(par_i){
        if(paste0("link_", par_i) %in% names(C_family_list)){
          return(C_family_list[[paste0("link_", par_i)]])
        } else{
          return(NA)
        }
      })
      dist_link_tmp$"Link function"[dist_link_tmp$"Parameter" %in% c("mu")] <- C_family_list$link
      return(dist_link_tmp)
    }
  })
  
  output$dist_link <- renderTable({
    dist_link_da()
  })
  
  #-------------------------------------------------
  # Predictors
  
  #------------------------
  # Main effects
  
  observe({
    updateSelectInput(session, "pred_mainNV_sel",
                      choices = c("Choose variables for nonvarying main effects ..." = "",
                                  setdiff(names(da()),
                                          c(input$outc_sel,
                                            input$pred_mainV_sel))),
                      selected = isolate(input$pred_mainNV_sel))
  })
  
  observe({
    updateSelectInput(session, "pred_mainV_sel",
                      choices = c("Choose variables for varying intercepts ..." = "",
                                  setdiff(names(da()),
                                          c(input$outc_sel,
                                            input$pred_mainNV_sel))),
                      selected = isolate(input$pred_mainV_sel))
  })
  
  #------------------------
  # Interactions
  
  observe({
    updateSelectInput(session, "pred_int_build",
                      choices = c("Choose variables for an interaction term ..." = "",
                                  input$pred_mainNV_sel,
                                  input$pred_mainV_sel),
                      selected = isolate(input$pred_int_build))
  })
  
  pred_int_rv <- reactiveValues()
  observeEvent(input$pred_int_add, {
    if(length(input$pred_int_build) > 1L){
      pred_int_rv$choices <- c(pred_int_rv$choices,
                               list(input$pred_int_build))
      pred_int_rv$choices <- pred_int_rv$choices[!duplicated(lapply(pred_int_rv$choices, sort))]
      pred_int_rv$choices_comma <- sapply(pred_int_rv$choices, paste, collapse = ",")
      updateSelectInput(session, "pred_int_sel",
                        choices = pred_int_rv$choices_comma,
                        selected = c(input$pred_int_sel,
                                     paste(input$pred_int_build, collapse = ",")))
      updateSelectInput(session, "pred_int_build",
                        choices = c("Choose variables for an interaction term ..." = "",
                                    input$pred_mainNV_sel,
                                    input$pred_mainV_sel))
    }
  })
  
  # Ensure that all variables involved in the interaction terms have a main effect (either
  # nonvarying or varying):
  observeEvent({
    input$pred_mainNV_sel
    input$pred_mainV_sel
  }, {
    pred_int_keep <- sapply(pred_int_rv$choices, function(x){
      all(x %in% c(input$pred_mainNV_sel,
                   input$pred_mainV_sel))
    })
    if(any(pred_int_keep)){
      pred_int_rv$choices <- pred_int_rv$choices[pred_int_keep]
      pred_int_rv$choices_comma <- pred_int_rv$choices_comma[pred_int_keep]
      updateSelectInput(session, "pred_int_sel",
                        choices = pred_int_rv$choices_comma,
                        selected = intersect(input$pred_int_sel,
                                             pred_int_rv$choices_comma))
    } else{
      pred_int_rv$choices <- NULL
      pred_int_rv$choices_comma <- NULL
      updateSelectInput(session, "pred_int_sel",
                        choices = character())
    }
  }, ignoreNULL = FALSE)
  
  #------------------------
  # Combination of all predictor terms
  
  C_pred <- reactive({
    if(all(c(input$pred_mainNV_sel,
             input$pred_mainV_sel) %in% names(da()))){
      pred_lst <- c(
        as.list(input$pred_mainNV_sel),
        as.list(input$pred_mainV_sel),
        pred_int_rv$choices[pred_int_rv$choices_comma %in% input$pred_int_sel]
      )
      if(length(input$pred_int_sel) > 0L){
        # Perform the following tasks (at the same time):
        #   - Expand interactions on the group-level side (in principle, this is not necessary as the
        #     "*" syntax (<predictor_1>*<predictor_2>) also works on the group-level side; however, for
        #     including correlations between the varying effects of a specific group-level term, the
        #     terms on the population-level side need to be grouped by the term on the group-level side)
        #   - For varying effects, add the corresponding nonvarying effects since the varying effects
        #     are assumed to have mean zero.
        # The first task is performed by applying combn() to m = 1L, ..., length(x_V) with "x_V"
        # containing the group-level terms of a given element of "pred_lst".
        # The second task is performed by additionally applying combn() to m = 0L when performing
        # the first task.
        pred_needsExpand <- sapply(pred_lst, function(x){
          sum(x %in% input$pred_mainV_sel) > 0L
        })
        if(any(pred_needsExpand)){ # This if() condition is not necessary, but included for better readability.
          pred_lst_toExpand <- pred_lst[pred_needsExpand]
          pred_lst_expanded <- do.call("c", lapply(pred_lst_toExpand, function(x){
            x_V <- intersect(x, input$pred_mainV_sel)
            x_V_lst_expanded <- unlist(lapply(c(0L, seq_along(x_V)), combn, x = x_V, simplify = FALSE),
                                       recursive = FALSE)
            x_NV <- intersect(x, input$pred_mainNV_sel)
            lapply(x_V_lst_expanded, "c", x_NV)
          }))
          pred_lst <- c(pred_lst[!pred_needsExpand],
                        pred_lst_expanded)
        }
        
        # Remove duplicates:
        pred_lst <- pred_lst[!duplicated(lapply(pred_lst, sort))]
        
        # By group-level term: Check each population-level term for being a "subterm" (lower-order
        # term) of a high-order term and if yes, remove it:
        pred_vec_comma <- sapply(pred_lst, function(x){
          x_V <- intersect(x, input$pred_mainV_sel)
          if(length(x_V) > 0L){
            return(paste(x_V, collapse = ","))
          } else{
            return(NA_character_)
          }
        })
        pred_vec_comma <- factor(pred_vec_comma, levels = unique(pred_vec_comma), exclude = NULL)
        pred_lst <- tapply(pred_lst, pred_vec_comma, function(x_lst){
          x_NV_lst <- lapply(x_lst, intersect, y = input$pred_mainNV_sel)
          x_isSubNV <- sapply(seq_along(x_NV_lst), function(idx){
            any(sapply(x_NV_lst[-idx], function(x_NV){
              all(x_NV_lst[[idx]] %in% x_NV)
            }))
          })
          return(x_lst[!x_isSubNV])
        }, simplify = FALSE)
        pred_lst <- unlist(pred_lst, recursive = FALSE, use.names = FALSE)
      }
      
      pred_DF <- do.call("rbind", lapply(pred_lst, function(x){
        x_NV <- intersect(x, input$pred_mainNV_sel)
        if(length(x_NV) > 0L){
          x_NV <- paste(x_NV, collapse = "*")
        } else{
          x_NV <- NA_character_
        }
        x_V <- intersect(x, input$pred_mainV_sel)
        if(length(x_V) > 0L){
          x_V <- paste(x_V, collapse = ":")
        } else{
          x_V <- NA_character_
        }
        data.frame("from_mainNV" = x_NV,
                   "from_mainV" = x_V)
      }))
      if(length(pred_DF) > 0L && nrow(pred_DF) > 0L){
        pred_DF$from_mainV <- factor(pred_DF$from_mainV, levels = unique(pred_DF$from_mainV), exclude = NULL)
        pred_DF <- aggregate(from_mainNV ~ from_mainV, pred_DF, function(x){
          paste(c("1", x[!is.na(x)]), collapse = " + ")
        }, na.action = na.pass)
      }
      return(pred_DF)
    } else{
      return(NULL)
    }
  })
  
  #------------------------
  # Predictor terms preview
  
  output$pred_view <- renderTable({
    C_pred()
  }, sanitize.colnames.function = function(x){
    x <- sub("^from_mainNV$", "Population-level effects", x)
    x <- sub("^from_mainV$", "Group-level effects", x)
    return(x)
  })
  
  #------------------------
  # Formula construction
  
  C_formula_char <- reactive({
    req(da(), input$outc_sel)
    if(input$outc_sel %in% names(da())){
      pred_DF <- C_pred()
      if(length(pred_DF) > 0L && nrow(pred_DF) > 0L){
        stopifnot(all(sapply(pred_DF, function(x){
          is.factor(x) || is.character(x) # Check this because apply() applied to a data.frame internally coerces to a matrix.
        })))
        formula_splitted <- apply(pred_DF, 1, function(x){
          isNA_V <- is.na(x["from_mainV"])
          if(!isNA_V){
            return(paste0("(", x["from_mainNV"], " | ", x["from_mainV"], ")"))
          } else{
            return(x["from_mainNV"])
          }
        })
      } else{
        formula_splitted <- "1"
      }
      return(paste(
        input$outc_sel,
        "~",
        paste(formula_splitted, collapse = " + ")
      ))
    } else{
      return(NULL)
    }
  })
  
  C_formula <- reactive({
    req(C_formula_char())
    as.formula(C_formula_char())
  })
  
  #------------------------
  # Formula preview
  
  output$formula_view <- renderText({
    C_formula_char()
  })
  
  #-------------------------------------------------
  # Prior
  
  #------------------------
  # Prior construction
  
  C_prior_rv <- reactiveValues(prior_default_obj = brms::empty_prior(),
                               prior_set_obj = brms::empty_prior())
  
  # Get default priors:
  observe({
    req(C_formula(), C_family())
    warn_orig <- options(warn = 1)
    warn_capt <- capture.output({
      C_prior_rv$prior_default_obj <- brms::get_prior(formula = C_formula(),
                                                      data = da(),
                                                      family = C_family())
    }, type = "message")
    options(warn = warn_orig$warn)
    if(length(warn_capt) > 0L){
      warn_capt <- unique(warn_capt)
      if(identical(warn_capt, "Warning: Rows containing NAs were excluded from the model.")){
        showNotification(
          paste("Warning: There are missing values in the data. The corresponding rows have been",
                "omitted in the construction of the default priors. They will also be omitted when",
                "running Stan (and also in the Stan data)."),
          duration = NA,
          type = "warning"
        )
      } else{
        showNotification(
          paste(warn_capt, collapse = "|"),
          duration = NA,
          type = "warning"
        )
      }
    }
  })
  
  # Reset the default priors if the dataset changes:
  observeEvent(da(), {
    C_prior_rv$prior_default_obj <- brms::empty_prior()
  })
  
  # Update the choices for "parameter class" (if necessary):
  observe({
    req(C_prior_rv$prior_default_obj)
    prior_class_choices <- unique(C_prior_rv$prior_default_obj$class)
    prior_class_choices <- setNames(prior_class_choices, prior_class_choices)
    prior_class_choices <- c("Choose parameter class ..." = "",
                             prior_class_choices)
    
    updateSelectInput(session, "prior_class_sel",
                      choices = prior_class_choices)
  })
  
  # Update the choices for "coefficient" (if necessary):
  observe({
    req(C_prior_rv$prior_default_obj)
    prior_coef_choices <- unique(c("", C_prior_rv$prior_default_obj$coef[
      C_prior_rv$prior_default_obj$class %in% input$prior_class_sel
    ]))
    prior_coef_choices <- setNames(prior_coef_choices, prior_coef_choices)
    names(prior_coef_choices)[prior_coef_choices == ""] <- "Choose coefficient or leave empty"
    
    updateSelectInput(session, "prior_coef_sel",
                      choices = prior_coef_choices)
  })
  
  # Update the choices for "group" (if necessary):
  observe({
    req(C_prior_rv$prior_default_obj)
    prior_group_choices <- unique(C_prior_rv$prior_default_obj$group[
      C_prior_rv$prior_default_obj$class %in% input$prior_class_sel &
        C_prior_rv$prior_default_obj$coef %in% input$prior_coef_sel
    ])
    if(identical(length(prior_group_choices), 0L)){
      prior_group_choices <- ""
    }
    prior_group_choices <- setNames(prior_group_choices, prior_group_choices)
    names(prior_group_choices)[prior_group_choices == ""] <- "Choose group or leave empty"
    
    if("" %in% prior_group_choices){
      prior_group_choices_sel <- prior_group_choices[prior_group_choices == ""]
    } else{
      prior_group_choices_sel <- intersect(prior_group_choices,
                                           isolate(input$prior_group_sel))
      prior_group_choices_sel <- setNames(prior_group_choices_sel, prior_group_choices_sel)
      if(identical(length(prior_group_choices_sel), 0L)){
        prior_group_choices_sel <- NULL
      }
    }
    
    updateSelectInput(session, "prior_group_sel",
                      choices = prior_group_choices,
                      selected = prior_group_choices_sel)
  })
  
  # Reset the user-specified priors if the default prior changes (the default prior changes
  # if the model formula changes (with the model formula also changing if the dataset changes) or 
  # if the distributional family for the outcome changes):
  observeEvent(C_prior_rv$prior_default_obj, {
    C_prior_rv$prior_set_obj <- brms::empty_prior()
  })
  
  # Add a user-specified prior if the user clicks the corresponding button:
  observeEvent(input$prior_add, {
    if(input$prior_class_sel != ""){
      C_prior_rv$prior_set_obj <-
        brms::set_prior(prior = input$prior_text,
                        class = input$prior_class_sel,
                        coef = input$prior_coef_sel,
                        group = input$prior_group_sel) +
        C_prior_rv$prior_set_obj
      C_prior_rv$prior_set_obj <- unique(C_prior_rv$prior_set_obj)
    }
  })
  
  # Reset the user-specified priors if the user clicks the corresponding button:
  observeEvent(input$prior_reset, {
    C_prior_rv$prior_set_obj <- brms::empty_prior()
  })
  
  #------------------------
  # Prior preview
  
  san_prior_tab_nms <- function(x){
    x <- sub("^prior$", "Prior", x)
    x <- sub("^class$", "Class", x)
    x <- sub("^coef$", "Coefficient", x)
    x <- sub("^group$", "Group", x)
    x <- sub("^resp$", "Response", x)
    x <- sub("^dpar$", "Distributional parameter", x)
    x <- sub("^nlpar$", "Non-linear parameter", x)
    x <- sub("^bound$", "Bound", x)
    return(x)
  }
  
  prior_colsToHide <- reactive({
    sapply(C_prior_rv$prior_default_obj, function(x){
      is.character(x) && all(x == "")
    }) &
      !grepl("^prior$|^class$|^coef$|^group$", names(C_prior_rv$prior_default_obj))
  })
  
  output$prior_default_view <- renderTable({
    C_prior_rv$prior_default_obj[, !prior_colsToHide()]
  }, sanitize.colnames.function = san_prior_tab_nms)
  
  output$prior_set_view <- renderTable({
    C_prior_rv$prior_set_obj[, !prior_colsToHide()]
  }, sanitize.colnames.function = san_prior_tab_nms)
  
  #-------------------------------------------------
  # Posterior
  
  #------------------------
  # Stan code
  
  C_stancode <- reactive({
    req(C_formula(), C_family())
    warn_orig <- options(warn = 1)
    warn_capt <- capture.output({
      C_stancode_tmp <- brms::make_stancode(formula = C_formula(),
                                            data = da(),
                                            family = C_family(),
                                            prior = C_prior_rv$prior_set_obj)
    }, type = "message")
    options(warn = warn_orig$warn)
    if(length(warn_capt) > 0L){
      warn_capt <- unique(warn_capt)
      if(identical(warn_capt, "Warning: Rows containing NAs were excluded from the model.")){
        showNotification(
          paste("Warning: There are missing values in the data. The corresponding rows will be",
                "omitted when running Stan (and also in the Stan data)."),
          duration = NA,
          type = "warning"
        )
      } else{
        showNotification(
          paste(warn_capt, collapse = "|"),
          duration = NA,
          type = "warning"
        )
      }
    }
    return(C_stancode_tmp)
  })
  
  output$stancode_view <- renderPrint({
    C_stancode()
  })
  
  output$stancode_download <- downloadHandler(
    filename = "shinybrms_stancode.stan",
    content = function(file){
      cat(C_stancode(), file = file)
    }
  )
  
  #------------------------
  # Stan data
  
  C_standata <- reactive({
    req(C_formula(), C_family())
    warn_orig <- options(warn = 1)
    warn_capt <- capture.output({
      C_standata_tmp <- brms::make_standata(formula = C_formula(),
                                            data = da(),
                                            family = C_family(),
                                            prior = C_prior_rv$prior_set_obj)
    }, type = "message")
    options(warn = warn_orig$warn)
    if(length(warn_capt) > 0L){
      warn_capt <- unique(warn_capt)
      if(identical(warn_capt, "Warning: Rows containing NAs were excluded from the model.")){
        showNotification(
          paste("Warning: There are missing values in the data. The corresponding rows have been",
                "omitted in the Stan data. They will also be omitted when running Stan."),
          duration = NA,
          type = "warning"
        )
      } else{
        showNotification(
          paste(warn_capt, collapse = "|"),
          duration = NA,
          type = "warning"
        )
      }
    }
    return(C_standata_tmp)
  })
  
  output$standata_view <- renderPrint({
    str(C_standata())
  })
  
  output$standata_download <- downloadHandler(
    filename = "shinybrms_standata.rds",
    content = function(file){
      saveRDS(C_standata(), file = file)
    }
  )
  
  #------------------------
  # Advanced options and run Stan
  
  args_brm <- reactive({
    req(C_formula(), C_family(),
        input$advOpts_cores,
        input$advOpts_chains,
        input$advOpts_iter,
        input$advOpts_thin,
        input$advOpts_init_r,
        input$advOpts_adapt_delta,
        input$advOpts_max_treedepth)
    args_brm_tmp <- list(
      formula = C_formula(),
      data = da(),
      family = C_family(),
      prior = C_prior_rv$prior_set_obj,
      ## Arguments which are preset by design of the UI:
      cores = min(input$advOpts_cores, input$advOpts_chains),
      chains = input$advOpts_chains,
      seed = input$advOpts_seed,
      iter = input$advOpts_iter,
      thin = input$advOpts_thin,
      inits = input$advOpts_inits,
      init_r = input$advOpts_init_r,
      open_progress = input$advOpts_open_progress,
      save_all_pars = input$advOpts_save_all_pars,
      save_warmup = input$advOpts_save_warmup,
      control = list(adapt_delta = input$advOpts_adapt_delta,
                     max_treedepth = input$advOpts_max_treedepth),
      ##
      ## Arguments which are fixed to a certain value:
      silent = TRUE,
      verbose = FALSE
      ##
    )
    if(!is.na(input$advOpts_warmup)){
      args_brm_tmp <- c(args_brm_tmp,
                        list(warmup = input$advOpts_warmup))
    }
    if(!is.na(input$advOpts_refresh)){
      args_brm_tmp <- c(args_brm_tmp,
                        list(refresh = input$advOpts_refresh))
    }
    return(args_brm_tmp)
  })
  
  C_fit <- eventReactive(input$run_stan, {
    req(args_brm())
    args_brm_copy <- args_brm()
    
    showNotification(
      paste("Stan is about to start sampling. Note that the C++ code needs to be compiled first",
            "and this may take a while."),
      duration = 60,
      type = "message"
    )
    
    # Some modifications needed to show the progress (see the source code of rstan::sampling()):
    if(args_brm_copy$open_progress){
      # For RStudio:
      RSTUDIO_orig <- Sys.getenv("RSTUDIO")
      if(identical(RSTUDIO_orig, "1")){
        Sys.setenv("RSTUDIO" = "")
      }
      
      # The progress browser:
      prog_browser <- getOption("shinybrms.prog_browser",
                                getOption("browser"))
      if(is.function(prog_browser) &&
         any(grepl("rs_browseURL", as.character(body(prog_browser))))){
        # In this case, "prog_browser" cannot be used (at least not without requiring the user to
        # perform some major modifications to the initialization of the R session), so use the
        # default browser stored in the environment variable "R_BROWSER":
        prog_browser <- Sys.getenv("R_BROWSER")
        if(identical(.Platform$OS.type, "windows") &&
           identical(prog_browser, "")){
          prog_browser <- NULL
        }
      }
      browser_orig <- options(browser = prog_browser)
      
      # Even show the progress if parallel::mclapply() (with forking) is intended to be used:
      if(identical(.Platform$OS.type, "unix")){
        if(!interactive()){
          tmp_stdout_txt <- tempfile(pattern = "shinybrms_stdout_", fileext = ".txt")
          sink(tmp_stdout_txt)
          sink_active <- TRUE
          cat("Refresh this page to see the sampling progress.",
              "Note that the C++ code needs to be compiled first and this may take",
              "a while.\n")
          tmp_stdout_html <- sub("\\.txt$", ".html", tmp_stdout_txt)
          rstan:::create_progress_html_file(tmp_stdout_html, tmp_stdout_txt)
          browseURL(paste0("file://", tmp_stdout_html))
        } else if(isatty(stdout())){
          sink(tempfile(pattern = "shinybrms_dummy_stdout", fileext = ".txt"))
          sink_active <- TRUE
        }
      }
    }
    
    # Get warnings directly when they occur:
    warn_orig <- options(warn = 1)
    
    # Run Stan (more precisely: brms::brm()):
    warn_capt <- capture.output({
      C_fit_tmp <- do.call(brms::brm, args = args_brm_copy)
    }, type = "message")
    
    # Reset all modified options and environment variables:
    options(warn = warn_orig$warn)
    if(exists("sink_active")) sink()
    if(exists("browser_orig")) options(browser = browser_orig$browser)
    if(!identical(Sys.getenv("RSTUDIO"), RSTUDIO_orig)) Sys.setenv("RSTUDIO" = RSTUDIO_orig)
    
    # Throw warnings if existing:
    if(length(warn_capt) > 0L){
      warn_capt <- unique(warn_capt)
      if(identical(warn_capt, "Warning: Rows containing NAs were excluded from the model.")){
        showNotification(
          paste("Warning: There are missing values in the data. The corresponding rows have been",
                "omitted in the Stan run."),
          duration = NA,
          type = "warning"
        )
      } else{
        showNotification(
          paste(warn_capt, collapse = "|"),
          duration = NA,
          type = "warning"
        )
      }
    }
    return(C_fit_tmp)
  })
  
  #------------------------
  # Output
  
  output$fit_date <- renderText({
    invisible(req(C_fit()))
    C_fit()$fit@date
  })
  
  output$smmry_view <- renderPrint({
    invisible(req(C_fit()))
    print(C_fit(), digits = 2, priors = TRUE, prob = 0.95, mc_se = FALSE)
  })
  
  output$stanout_download <- downloadHandler(
    filename = function(){
      if(identical(input$stanout_download_sel, "draws_mat_csv")){
        return("shinybrms_post_draws_mat.csv")
      } else{
        return(paste0(switch(input$stanout_download_sel,
                             "brmsfit_obj" = "shinybrms_brmsfit",
                             "draws_mat_obj" = "shinybrms_post_draws_mat",
                             "draws_arr_obj" = "shinybrms_post_draws_arr"),
                      ".rds"))
      }
    },
    content = function(file){
      if(identical(input$stanout_download_sel, "draws_mat_csv")){
        write.csv(as.matrix(C_fit()),
                  file = file,
                  row.names = FALSE)
      } else{
        saveRDS(switch(input$stanout_download_sel,
                       "brmsfit_obj" = C_fit(),
                       "draws_mat_obj" = as.matrix(C_fit()),
                       "draws_arr_obj" = as.array(C_fit())),
                file = file)
      }
    }
  )
  
  observeEvent(input$act_launch_shinystan, {
    invisible(req(C_fit()))
    if(requireNamespace("shinystan", quietly = TRUE)){
      if(requireNamespace("callr", quietly = TRUE)){
        # The browser for "shinystan":
        shinystan_browser <- getOption("shinybrms.shinystan_browser",
                                       getOption("browser"))
        if(is.function(shinystan_browser) &&
           any(grepl("rs_browseURL", as.character(body(shinystan_browser))))){
          # In this case, "shinystan_browser" cannot be used (at least not without requiring the
          # user to perform some major modifications to the initialization of the R session), so use
          # the default browser stored in the environment variable "R_BROWSER":
          shinystan_browser <- Sys.getenv("R_BROWSER")
          if(identical(.Platform$OS.type, "windows") &&
             identical(shinystan_browser, "")){
            shinystan_browser <- NULL
          }
        }
        
        # Call "shinystan" from an external R process (needed to allow opening another Shiny app
        # (here "shinystan") from within this Shiny app ("shinybrms")):
        callr::r(
          function(brmsfit_obj, browser_callr, seed_callr){
            browser_callr_orig <- options(browser = browser_callr)
            assign("y", brms::get_y(brmsfit_obj), envir = .GlobalEnv)
            if(!is.vector(y)) assign("y", as.vector(y), envir = .GlobalEnv)
            set.seed(seed_callr)
            assign("y_rep", brms::posterior_predict(brmsfit_obj), envir = .GlobalEnv)
            shinystan::launch_shinystan(brmsfit_obj,
                                        rstudio = FALSE)
            options(browser = browser_callr_orig$browser)
            return(invisible(TRUE))
          },
          args = list(brmsfit_obj = C_fit(),
                      browser_callr = shinystan_browser,
                      seed_callr = input$seed_PPD)
        )
      } else{
        showNotification(
          "Package \"callr\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
      }
    } else{
      showNotification(
        "Package \"shinystan\" needed. Please install it.",
        duration = NA,
        type = "error"
      )
    }
  })
  
  #------------------------
  # Quit app
  
  observe({
    if(identical(input$navbar_ID, "quit_app")){
      stopApp()
    }
  })
  
  session$onSessionEnded(
    function(){
      stopApp()
    }
  )
  
}

####################################################################################################
# Call to shinyApp()
####################################################################################################

shinyApp(ui = ui, server = server)
