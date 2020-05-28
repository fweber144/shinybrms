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
                 "to clear out the name of the example dataset from the field \"Choose example dataset\"."),
        helpText(HTML(paste0("The following data entries are recognized as missing values: empty ",
                             "(i.e. nothing, not even a whitespace), whitespace, ", code("NA"),
                             ", ", code("."), " (dot)."))),
        # Horizontal line (first one, so set the global hr() style here):
        hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
        h4("Example dataset"),
        selectInput("ex_da_sel", "Choose example dataset",
                    choices = c("Choose ..." = "",
                                "Arabidopsis (from package \"lme4\")" = "Arabidopsis",
                                "bodyfat (online resource; see page \"Links\")" = "bodyfat",
                                "diabetes (online resource; see page \"Links\")" = "diabetes",
                                "grouseticks (from package \"lme4\")" = "grouseticks",
                                "kidiq (from package \"rstanarm\")" = "kidiq",
                                "mesquite (online resource; see page \"Links\")" = "mesquite",
                                "mtcars" = "mtcars",
                                "Prostate (from package \"lasso2\")" = "Prostate",
                                "Puromycin" = "Puromycin",
                                "quine (from package \"MASS\")" = "quine",
                                "Rabbit (from package \"MASS\")" = "Rabbit",
                                "roaches (from package \"rstanarm\")" = "roaches",
                                "sleepstudy (from package \"lme4\")" = "sleepstudy",
                                "ToothGrowth" = "ToothGrowth",
                                "winequality-red (online resource; see page \"Links\")" = "winequality-red"),
                    selectize = TRUE),
        # Horizontal line:
        hr(),
        h4("Upload a dataset"),
        fileInput("file_upload", "Choose file",
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
        # Horizontal line:
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
      # "Subsections",
      tabPanel(
        "Outcome",
        titlePanel("Outcome"),
        br(),
        helpText("Define the outcome (the dependent variable) and the distributional family for this outcome."),
        selectInput("outc_sel", "Choose outcome:",
                    choices = c("Choose ..." = ""),
                    selectize = TRUE),
        selectInput("dist_sel", "Choose distributional family for the outcome:",
                    choices = list("Choose ..." = c("Choose ..." = ""),
                                   "Continuous outcome:" = c("Gaussian (normal)" = "gaussian"),
                                   "Binary outcome:" = c("Bernoulli with logit link" = "bernoulli"),
                                   "Count data outcome:" = c("Negative binomial" = "negbinomial")),
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
        helpText("Define the predictors (the independent variables). More specifically, you may define",
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
        # # Horizontal line:
        # hr(),
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
                   "non-varying main effects shall be added."),
          varSelectInput("pred_mainNV_sel", "Choose variables for non-varying main effects:",
                         data = data.frame(),
                         multiple = TRUE,
                         selectize = TRUE),
          h4("Varying intercepts"),
          helpText("Start typing or click into the field below to choose variables for which",
                   "varying intercepts shall be added."),
          varSelectInput("pred_mainV_sel", "Choose variables for varying intercepts:",
                         data = data.frame(),
                         multiple = TRUE,
                         selectize = TRUE)
        ),
        # # Horizontal line:
        # hr(),
        wellPanel(
          h3("Interaction effects"),
          helpText("Currently, only interactions between predictor variables with non-varying main effects are supported.",
                   "Start typing or click into the field below to choose variables for which an",
                   "interaction shall be added. Confirm this interaction by pressing the",
                   "\"Add interaction\" button. All interactions which have been added are",
                   "listed in the box below the \"Add interaction\" button. You may reset", em("all"),
                   "interactions by pressing the \"Reset all interactions\" button."),
          varSelectInput("pred_int_sel", "Choose variables for an interaction:",
                         data = data.frame(),
                         multiple = TRUE,
                         selectize = TRUE),
          actionButton("pred_int_add", "Add interaction"),
          br(),
          br(),
          strong("Added interactions (\"NULL\" means that no interactions have been added yet):"),
          verbatimTextOutput("pred_int_out", placeholder = TRUE),
          actionButton("pred_int_reset", "Reset all interactions")
        ),
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
      " from package",
      a(HTML("<strong>brms</strong>"),
        href = "https://CRAN.R-project.org/package=brms",
        target = "_blank"),
      "."
    ))),
    # Horizontal line:
    hr(),
    h3("Default priors"),
    br(),
    strong("Default priors for the parameters belonging to the current likelihood:"),
    tableOutput("prior_default_view"),
    helpText("An empty field in column \"prior\" denotes a flat prior over the domain of the",
             "corresponding parameter."),
    # Horizontal line:
    hr(),
    h3("Custom priors"),
    sidebarLayout(
      sidebarPanel(
        h4("Specification of custom priors"),
        br(),
        selectInput("prior_class_sel", "Parameter class (may consist of a single parameter):",
                    choices = c("Choose ..." = ""),
                    selectize = TRUE),
        selectInput("prior_coef_sel", "Coefficient (leave empty for using all coefficients belonging to the selected parameter class):",
                    choices = c("Choose or leave empty" = ""),
                    selectize = TRUE),
        strong("Group:"),
        helpText("Varying effects are not supported yet."),
        textInput("prior_text", "Prior distribution (in Stan language or leave empty for using a flat prior):",
                  value = "",
                  width = "400px",
                  placeholder = "Enter prior distribution in Stan language or leave empty ..."),
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
        helpText("An empty field in column \"prior\" denotes a flat prior over the domain of the",
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
    # # Horizontal line:
    # hr(),
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
    # # Horizontal line:
    # hr(),
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
    # # Horizontal line:
    # hr(),
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
                              value = NA, step = 1L, min = 0L), # value = 1000L
                 numericInput("advOpts_thin", "Thinning rate:",
                              value = 1L, step = 1L, min = 1L)),
          column(5, offset = 1, # offset = 2,
                 radioButtons("advOpts_inits", "Initial values:",
                              choices = list("Random" = "random", "Zero" = "0"),
                              inline = TRUE),
                 numericInput("advOpts_init_r", "\"init_r\" (only relevant for random initial values):",
                              value = 2, step = 0.1, min = 0),
                 numericInput("advOpts_adapt_delta", "\"adapt_delta\":",
                              value = 0.95, step = 0.01, min = 0, max = 1),
                 numericInput("advOpts_max_treedepth", "\"max_treedepth\":",
                              value = 15L, step = 1L, min = 1L),
                 ### If "control" list shall be constructed more flexibly:
                 # selectInput("advOpts_control_name", "Name of \"control\" element:",
                 #             choices = c("Choose ..." = "",
                 #                         "\"adapt_delta\"" = "adapt_delta",
                 #                         "\"max_treedepth\"" = "max_treedepth"),
                 #             selectize = TRUE),
                 # textInput("advOpts_control_text", "Value for \"control\" element:",
                 #           value = "",
                 #           # width = "400px",
                 #           placeholder = "Enter value for the \"control\" element chosen above ..."),
                 ###
                 checkboxInput("advOpts_open_progress", "Open progress",
                               value = TRUE),
                 numericInput("advOpts_refresh", "Progress-refreshing step size:",
                              value = NA, step = 1L, min = 0L), # value = 200L
                 checkboxInput("advOpts_save_all_pars", "\"save_all_pars\"",
                               value = FALSE),
                 checkboxInput("advOpts_save_warmup", "Save warmup",
                               value = TRUE))
        )
      )
    ),
    # # Horizontal line:
    # hr(),
    wellPanel(
      h3("Run Stan"),
      helpText("Note: If the advanced option \"Open progress\" is selected (as per default),",
               "Windows users having Firefox set as their default web browser may need to manually",
               "copy the link to the Stan HTML progress file which is automatically opening up and",
               "paste this link into a different web browser for viewing the progress file there."),
      actionButton("run_stan", "Run Stan (may take a while)")
    ),
    # # Horizontal line:
    # hr(),
    wellPanel(
      h3("Output"),
      strong("Date and time when Stan run was finished:"),
      textOutput("fit_date"),
      br(),
      strong("Summary:"),
      verbatimTextOutput("smmry_view", placeholder = TRUE),
      selectInput("stanout_download_sel", "Choose output object to download:",
                  choices = c("\"brmsfit\" object" = "brmsfit_obj",
                              "Matrix of posterior draws (CSV file)" = "draws_mat_csv",
                              "Matrix of posterior draws (R object)" = "draws_mat_obj",
                              "Array of posterior draws (R object)" = "draws_arr_obj"),
                  selectize = TRUE),
      helpText(HTML(paste0("The most comprehensive output object is the \"brmsfit\" object which ",
                           "is the output from ", code("brms::brm()"), ", the central function ",
                           "for inferring the posterior."))),
      downloadButton("stanout_download", "Download output object"),
      br(),
      br(),
      h4("Interactive output inspection using package", strong("shinystan")),
      numericInput("seed_PPD",
                   paste("Seed for draws from posterior predictive distribution",
                         "(leave empty to use a random seed):"),
                   value = NULL, step = 1L),
      actionButton("act_launch_shinystan", HTML(paste("Launch", strong("shinystan"), "(may take a while)"))),
      helpText(
        "Note: In the", strong("shinystan"), "app, the parameter names given by", strong("brms"), "are used.",
        "These are as follows:",
        tags$ul(
          tags$li("\"b_Intercept\" is the intercept (with respect to the non-centered predictors)."),
          tags$li("The parameters starting with \"b_\" are the regression coefficients."),
          tags$li(HTML(paste("All other parameters are parameters specific to the chosen distributional family",
                             "for the outcome (see page \"Likelihood\" &rarr; \"Outcome\").")))
        )
      )
    )
  ),
  navbarMenu(
    "More",
    tabPanel(
      "About",
      titlePanel("About \"shinybrms\""),
      br(),
      # # Horizontal line:
      # hr(),
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
      # # Horizontal line:
      # hr(),
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
      # # Horizontal line:
      # hr(),
      wellPanel(
        h3("Software (without R packages)"),
        tags$ul(
          tags$li(a("R", href = "https://www.R-project.org/", target = "_blank")),
          tags$li(a("Stan", href = "https://mc-stan.org/", target = "_blank"))
        )
      ),
      # # Horizontal line:
      # hr(),
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
      ),
      # # Horizontal line:
      # hr(),
      wellPanel(
        h3("Example datasets (online resources)"),
        tags$ul(
          tags$li(a("bodyfat",
                    href = "https://raw.githubusercontent.com/avehtari/modelselection/master/bodyfat.txt",
                    target = "_blank")),
          tags$li(a("diabetes",
                    href = "https://raw.githubusercontent.com/avehtari/modelselection/master/diabetes.csv",
                    target = "_blank")),
          tags$li(a("mesquite",
                    href = "https://raw.githubusercontent.com/avehtari/modelselection/master/mesquite.dat",
                    target = "_blank")),
          tags$li(a("winequality-red",
                    href = "https://raw.githubusercontent.com/avehtari/modelselection/master/winequality-red.csv",
                    target = "_blank"))
        )
      )
    ),
    tabPanel(
      "Help",
      titlePanel("Help"),
      br(),
      # # Horizontal line:
      # hr(),
      wellPanel(
        h3("Software"),
        HTML(paste0("For help concerning the software used in the ", strong("shinybrms"), " app ",
                    "(including the R package ", strong("shinybrms"), "), see ",
                    "the \"More\" &rarr; \"Links\" page."))
      ),
      # # Horizontal line:
      # hr(),
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
        return(get("Arabidopsis", envir = tmp_env))
      } else{
        showNotification(
          "Package \"lme4\" needed. Please install it.",
          duration = NA,
          type = "error"
        )
        return(NULL)
      }
    } else if(identical(input$ex_da_sel, "bodyfat")){
      return(read.table("https://raw.githubusercontent.com/avehtari/modelselection/master/bodyfat.txt",
                        header = TRUE, sep = ";", dec = ".")) # , quote = ""
    } else if(identical(input$ex_da_sel, "diabetes")){
      return(read.csv("https://raw.githubusercontent.com/avehtari/modelselection/master/diabetes.csv",
                      header = TRUE, sep = ",", dec = ".")) # , quote = ""
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
    } else if(identical(input$ex_da_sel, "mesquite")){
      return(read.table("https://raw.githubusercontent.com/avehtari/modelselection/master/mesquite.dat",
                        header = TRUE, sep = "", dec = ".")) # , quote = ""
    } else if(identical(input$ex_da_sel, "mtcars")){
      return(mtcars)
    } else if(identical(input$ex_da_sel, "Prostate")){
      if(requireNamespace("lasso2", quietly = TRUE)){
        tmp_env <- new.env()
        data(Prostate, package = "lasso2", envir = tmp_env)
        return(get("Prostate", envir = tmp_env))
      } else{
        showNotification(
          "Package \"lasso2\" needed. Please install it.",
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
    } else if(identical(input$ex_da_sel, "winequality-red")){
      return(read.csv("https://raw.githubusercontent.com/avehtari/modelselection/master/winequality-red.csv",
                      header = TRUE, sep = ";", dec = ".")) # , quote = ""
    } else{
      # NOTE: input$file_upload will be NULL initially.
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
                      choices = c("Choose ..." = "",
                                  names(da())))
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
        data.frame("parameter" = character(),
                   "link function" = character(),
                   check.names = FALSE)
      )
    } else{
      C_family_list <- C_family()
      dist_link_tmp <- data.frame("parameter" = C_family_list$dpars,
                                  "link function" = NA,
                                  check.names = FALSE)
      dist_link_tmp$"link function" <- sapply(dist_link_tmp$"parameter", function(par_i){
        if(paste0("link_", par_i) %in% names(C_family_list)){
          return(C_family_list[[paste0("link_", par_i)]])
        } else{
          return(NA)
        }
      })
      dist_link_tmp$"link function"[dist_link_tmp$"parameter" %in% c("mu")] <- C_family_list$link
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
    updateVarSelectInput(session, "pred_mainNV_sel",
                         data = da())
  })

  observe({
    updateVarSelectInput(session, "pred_mainV_sel",
                         data = da())
  })

  #------------------------
  # Interactions

  observe({
    updateVarSelectInput(session, "pred_int_sel",
                         data = da())
  })

  pred_int_rv <- reactiveValues()
  observeEvent(input$pred_int_add, {
    if(length(input$pred_int_sel) > 1L){
      pred_int_rv$pred_int <- c(pred_int_rv$pred_int,
                                paste(as.character(input$pred_int_sel), collapse = ":"))
      pred_int_rv$pred_int <- unique(pred_int_rv$pred_int)
    }
  })

  output$pred_int_out <- renderPrint({
    pred_int_rv$pred_int
  })

  observeEvent(input$pred_int_reset, {
    pred_int_rv$pred_int <- NULL
  })

  #------------------------
  # Formula construction

  pred_mainV <- reactive({
    if(length(input$pred_mainV_sel) > 0L){
      return(paste0("(1|", as.character(input$pred_mainV_sel), ")"))
    } else{
      return(character())
    }
  })

  C_formula_char <- reactive({
    req(input$outc_sel)
    if(all(c(input$outc_sel,
             as.character(input$pred_mainNV_sel),
             as.character(input$pred_mainV_sel)) %in% names(da()))){
      return(paste(input$outc_sel,
                   "~",
                   paste(c("1",
                           as.character(input$pred_mainNV_sel),
                           pred_mainV(),
                           pred_int_rv$pred_int),
                         collapse = " + ")))
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

  output$formula_view <- renderPrint({
    cat(C_formula_char())
  })

  #-------------------------------------------------
  # Prior

  #------------------------
  # Prior construction

  C_prior_rv <- reactiveValues(prior_default_obj = brms::empty_prior(),
                               prior_set_obj = brms::empty_prior())

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

  observeEvent({
    da()
  }, {
    C_prior_rv$prior_default_obj <- brms::empty_prior()
  })

  observe({
    req(C_prior_rv$prior_default_obj)
    prior_class_choices_add <- unique(C_prior_rv$prior_default_obj$class)
    prior_class_choices_add <- setNames(prior_class_choices_add, prior_class_choices_add)
    prior_class_choices <- c("Choose ..." = "",
                             prior_class_choices_add)

    updateSelectInput(session, "prior_class_sel",
                      choices = prior_class_choices)
  })

  observe({
    req(C_prior_rv$prior_default_obj)
    prior_coef_choices_add <- unique(C_prior_rv$prior_default_obj$coef[
      C_prior_rv$prior_default_obj$class %in% input$prior_class_sel
    ])
    prior_coef_choices_add <- setNames(prior_coef_choices_add, prior_coef_choices_add)
    prior_coef_choices <- c("Choose or leave empty" = "",
                            prior_coef_choices_add)

    updateSelectInput(session, "prior_coef_sel",
                      choices = prior_coef_choices)
  })

  # Reset the user-specified priors if the default prior has changed (the default prior changes
  # when the dataset or the model formula changes (with "model formula" including the family)):
  observeEvent(C_prior_rv$prior_default_obj, {
    C_prior_rv$prior_set_obj <- brms::empty_prior()
  })

  observeEvent(input$prior_add, {
    if(input$prior_class_sel != ""){
      C_prior_rv$prior_set_obj <-
        brms::set_prior(prior = input$prior_text,
                        class = input$prior_class_sel,
                        coef = input$prior_coef_sel) +
        C_prior_rv$prior_set_obj
      C_prior_rv$prior_set_obj <- unique(C_prior_rv$prior_set_obj)
    }
  })

  observeEvent(input$prior_reset, {
    C_prior_rv$prior_set_obj <- brms::empty_prior()
  })

  #------------------------
  # Prior preview

  output$prior_default_view <- renderTable({
    C_prior_rv$prior_default_obj
  })

  output$prior_set_view <- renderTable({
    C_prior_rv$prior_set_obj
  })

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

    # Some modifications needed to show the progress (see the source code of
    # rstan::sampling()):
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
        # In this case, "prog_browser" cannot be used (at least not without
        # requiring the user to perform some major modifications to the
        # initialization of the R session), so use the default browser stored
        # in the environment variable "R_BROWSER":
        prog_browser <- Sys.getenv("R_BROWSER")
        if(identical(.Platform$OS.type, "windows") &&
           identical(prog_browser, "")){
          prog_browser <- NULL
        }
      }
      browser_orig <- options(browser = prog_browser)

      # Even show the progress if parallel::mclapply() (with forking) is
      # intended to be used:
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
          # In this case, "shinystan_browser" cannot be used (at least not without
          # requiring the user to perform some major modifications to the
          # initialization of the R session), so use the default browser stored
          # in the environment variable "R_BROWSER":
          shinystan_browser <- Sys.getenv("R_BROWSER")
          if(identical(.Platform$OS.type, "windows") &&
             identical(shinystan_browser, "")){
            shinystan_browser <- NULL
          }
        }

        # Call "shinystan" from an external R process (needed to allow opening
        # another Shiny app ("shinystan") from within this Shiny app ("shinybrms")):
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
