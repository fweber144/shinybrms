# shinybrms: Graphical User Interface ('shiny' App) for 'brms'
# Copyright (C) 2021  Frank Weber
#   
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Preparations and global definitions -------------------------------------

library(shiny)

# Increase size limit for file uploads (necessary especially for `brmsfit`
# objects stored in RDS files); default is 50 MB if unset:
options(shiny.maxRequestSize = getOption("shiny.maxRequestSize", 50 * 1024^2))

# Needed to prevent occasional RStudio crashes when starting the Stan run with
# "rstan" version >= 2.21.1:
if (packageVersion("rstan") >= "2.21.1") {
  rstan::rstan_options("javascript" = FALSE)
}

# If in test mode, turn off locale-specific sorting:
if (isTRUE(getOption("shiny.testmode"))) {
  lc_collate_orig <- Sys.getlocale("LC_COLLATE")
  Sys.setlocale("LC_COLLATE", "C")
}

san_prior_tab_nms <- function(x) {
  x <- sub("^prior$", "Prior", x)
  x <- sub("^class$", "Class", x)
  x <- sub("^coef$", "Coefficient", x)
  x <- sub("^group$", "Group", x)
  x <- sub("^resp$", "Response", x)
  x <- sub("^dpar$", "Distributional parameter", x)
  x <- sub("^nlpar$", "Non-linear parameter", x)
  x <- sub("^bound$", "Bound", x)
  x <- sub("^source$", "Source", x)
  return(x)
}

# Stan function names which may be used for specifying a prior distribution:
prior_stan_fun <- c(
  "normal",
  "std_normal",
  "exp_mod_normal",
  "skew_normal",
  "student_t",
  "cauchy",
  "double_exponential",
  "logistic",
  "gumbel",
  "skew_double_exponential",
  ### Requiring a lower bound (which is checked by brms:::check_prior_content()):
  "lognormal",
  "chi_square",
  "inv_chi_square",
  "scaled_inv_chi_square",
  "exponential",
  "gamma",
  "inv_gamma",
  "weibull",
  "frechet",
  "rayleigh",
  "pareto",
  "pareto_type_2",
  "wiener",
  ### 
  ### Requiring a lower bound and an upper bound (which is checked by brms:::check_prior_content()):
  "beta",
  "beta_proportion",
  "von_mises",
  "uniform"
  ### 
)

# brms function names which may be used for specifying a prior distribution:
prior_brms_fun <- c(
  "horseshoe",
  "lasso",
  "constant",
  ### Requiring a simplex constraint:
  "dirichlet",
  ### 
  ### For parameters of class "cor" (only used by brms; has no Stan function equivalent):
  "lkj",
  ### 
  ### Requiring a Cholesky-factor-of-correlation-matrix constraint:
  "lkj_corr_cholesky",
  ### 
  ### Requiring a correlation-matrix constraint:
  "lkj_corr"
  ### 
)

# Dummy hash for the case of no data:
da_hash_no_data <- c(
  paste("This is not a hash, but just a dummy string which is not identical to",
        "any hash (and safer than the default `NULL` in reactiveVal())."),
  "And this is just a dummy string to get length > 1L."
)

# Allowed symbols for "Custom summary":
cust_allow_all <- c(as.character(0:9), " ", ".", "(", ")",
                    getGroupMembers("Arith"),
                    getGroupMembers("Compare"),
                    getGroupMembers("Logic"), "!",
                    getGroupMembers("Math"), "pmax", "pmin")
# Escape special characters:
cust_allow_spec <- c(".", "|", "(", ")", "^", "*", "+") # , "\\", "[", "{", "$", "?"
cust_allow_grp <- cust_allow_all
for (char_i in cust_allow_spec) {
  cust_allow_grp <- sub(char_i, paste0("\\", char_i), cust_allow_grp, fixed = TRUE)
}

# Empty "Custom summary" table:
cust_smmry_empty <- setNames(
  cbind(data.frame(character()), as.data.frame(matrix(0, nrow = 0, ncol = 8))),
  sub("^Q50$",
      "median",
      c("Name",
        paste0("Q", sub("\\.0$", "", 100 * c(0.025, 0.25, 0.5, 0.75, 0.975))),
        "MAD", "mean", "SD"))
)

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  "shinybrms", id = "navbar_ID",
  
  ## Home -------------------------------------------------------------------
  
  tabPanel(
    "Home",
    titlePanel("Home"),
    br(),
    # hr(),
    wellPanel(
      h3("Welcome to the", strong("shinybrms"), "app!",
         style = "text-align:center"),
      # br(),
      h4("Description"),
      p("This",
        a(HTML(paste(strong("shiny"))),
          href = "https://shiny.rstudio.com/",
          target = "_blank"),
        "app is part of the",
        a("R", href = "https://www.R-project.org/", target = "_blank"),
        "package",
        a(HTML(paste(strong("shinybrms"))),
          href = "https://fweber144.github.io/shinybrms/",
          target = "_blank"),
        "and allows to fit Bayesian regression models using the R package",
        a(HTML(paste(strong("brms"))),
          href = "https://paul-buerkner.github.io/brms/",
          target = "_blank"),
        "which in turn relies on",
        a("Stan", href = "https://mc-stan.org/", target = "_blank", .noWS = "after"),
        ". More specifically,", strong("brms"), "offers two backends: The",
        a(HTML(paste(strong("rstan"))),
          href = "https://mc-stan.org/rstan/",
          target = "_blank"),
        "or the",
        a(HTML(paste(strong("cmdstanr"))),
          href = "https://mc-stan.org/cmdstanr/",
          target = "_blank"),
        "R package. Both backends are supported by",
        strong("shinybrms", .noWS = "after"), "."),
      h4("Bayesian regression models"),
      p("The fundamental principle of Bayesian statistics is", em("Bayes' theorem", .noWS = "after"),
        ". In the context relevant for this app, Bayes' theorem may be reduced to the statement",
        "that the joint posterior distribution of all parameters in the regression model is",
        "proportional to the product of their joint prior distribution and the likelihood.",
        "(Here and in the following, the term \"distribution\" is used interchangeably for a",
        "probability distribution and the corresponding probability density function or",
        "probability mass function.)",
        "The posterior distribution reflects the knowledge about the parameters", em("after"),
        "having seen the data, whereas",
        "the prior distribution reflects the knowledge about the parameters", em("before"),
        "having seen the data.",
        "The likelihood corresponds to the distribution of the outcome conditional on the",
        "parameters and the predictors. (Note that the likelihood is considered as a function of",
        "the parameters.)",
        "Thus, after having specified the likelihood and the prior (distribution), the aim of a",
        "Bayesian data analysis is to infer the posterior (distribution) and then perform",
        "analyses based on the posterior, e.g., plotting marginal posterior distributions and",
        "calculating their 2.5%, 50%, and 97.5% quantiles."),
      p("For a more thorough introduction to Bayesian statistics in general as well as",
        "Bayesian regression models in particular, the following textbooks might",
        "be helpful:",
        tags$ul(
          tags$li("McElreath R (2020).",
                  em("Statistical Rethinking: A Bayesian Course with Examples in R and Stan", .noWS = "after"),
                  ". 2nd ed. Boca Raton, FL: CRC Press."),
          tags$li("Gelman A, Carlin JB, Stern HS, Dunson DB, Vehtari A, and Rubin DB (2014).",
                  em("Bayesian Data Analysis", .noWS = "after"),
                  ". 3rd ed. Boca Raton, FL: CRC Press.")
        )),
      h4("Notes"),
      p("The structure of the", strong("shinybrms"), "app follows the principle described in the previous",
        "section \"Bayesian regression models\":",
        "The three main pages in the navigation bar above are",
        HTML(paste(actionLink("likelihood_link1", "Likelihood")), .noWS = "after"), ",",
        HTML(paste(actionLink("prior_link1", "Prior")), .noWS = "after"), ", and",
        HTML(paste(actionLink("posterior_link1", "Posterior")), .noWS = "after"), ".",
        "Before starting with these pages, the dataset has to be uploaded on page",
        actionLink("data_link1", "Data"),
        "(even though",
        "for testing purposes, you may also choose an example dataset there). Every page should provide",
        "help texts where necessary. If you need more help, if you want to suggest improvements, or if you",
        "found a bug, please open an issue on",
        a("GitHub", href = "https://github.com/fweber144/shinybrms/issues", target = "_blank", .noWS = "after"),
        ". Some basic information about", strong("shinybrms"), "as well as some legal information",
        "may be found on page",
        HTML(paste(actionLink("about_link1", "About")), .noWS = "after"), ".",
        "Links to the software relevant for this app are given on page",
        HTML(paste(actionLink("links_link1", "Links")), .noWS = "after"), ".",
        "References for literature cited throughout the app may be found on page",
        HTML(paste(actionLink("references_link1", "References")), .noWS = "after"), "."),
      p("Furthermore, the following conventions are used throughout this app:",
        tags$ul(
          tags$li("Names of R packages are written in bold (e.g.,", strong("brms", .noWS = "after"), ")."),
          tags$li("Names of R functions are given according to the scheme",
                  code("<package>::<function>()"), "with", code("<package>"), "denoting the R package which",
                  "contains the", code("<function>", .noWS = "after"), ". Where possible, this scheme is hyperlinked.")
        ))
    ),
    # hr(),
    icon = icon("home")
  ),
  
  ## Data -------------------------------------------------------------------
  
  tabPanel(
    "Data",
    titlePanel("Data"),
    br(),
    sidebarLayout(
      sidebarPanel(
        helpText(
          p("Either choose an example dataset or upload a file (preferably",
            "*.csv, *.txt, or *.dat) containing your own dataset. In either",
            "case, a preview of the dataset will be shown in the main panel on",
            "the right."),
          p("If you want to upload a dataset after having chosen an",
            "example dataset, you have to clear the input field \"Choose",
            "example dataset ...\" first."),
          p("The following data entries are recognized as missing values: empty ",
            "(i.e. nothing, not even a whitespace), whitespace, ", code("NA"),
            ", ", code("."), " (dot).")
        ),
        # Horizontal line (first one, so set the global hr() style here):
        hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
        h4("Example dataset"),
        helpText("Some example datasets are loaded from third-party R packages (",
                 a(HTML(paste(strong("lme4"))),
                   href = "https://CRAN.R-project.org/package=lme4",
                   target = "_blank",
                   .noWS = "outside"),
                 ", ",
                 a(HTML(paste(strong("MASS"))),
                   href = "https://CRAN.R-project.org/package=MASS",
                   target = "_blank",
                   .noWS = "after"),
                 ", or ",
                 a(HTML(paste(strong("rstanarm"))),
                   href = "https://mc-stan.org/rstanarm/",
                   target = "_blank",
                   .noWS = "after"),
                 ")."),
        selectInput("ex_da_sel", NULL,
                    choices = c(
                      "Choose example dataset ..." = "",
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
                      "ToothGrowth" = "ToothGrowth"
                    ),
                    selectize = TRUE),
        hr(),
        h4("Upload a dataset"),
        fileInput("data_upload", "Choose file:",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values",
                             "text/plain",
                             ".csv",
                             ".txt",
                             ".dat"),
                  buttonLabel = "Browse ..."),
        strong("Header:"),
        checkboxInput("header", "The file has a header containing the column names", TRUE),
        radioButtons("sep", "Separator symbol:",
                     choices = c("Comma" = ",",
                                 "Semicolon" = ";",
                                 "Tab" = "\t",
                                 "Whitespace" = "")),
        radioButtons("quote", "Quote symbol:",
                     choices = c("None" = "",
                                 "Double quote" = '"',
                                 "Single quote" = "'"),
                     selected = '"'),
        radioButtons("dec", "Decimal symbol:",
                     choices = c("Point" = ".",
                                 "Comma" = ",")),
        hr(),
        h4("Preview"),
        radioButtons("preview_type_radio", "Type of preview:",
                     choices = c("Dataset" = "datas",
                                 "Structure" = "struc")),
        radioButtons("preview_rows_radio", "Rows to show (only for preview type \"Dataset\"):",
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
  
  ## Likelihood -------------------------------------------------------------
  
  tabPanel(
    "Likelihood",
    titlePanel("Likelihood"),
    br(),
    navlistPanel(
      ### Outcome ---------------------------------------------------------------
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
                      "Choose distributional family ..." = "",
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
        helpText(
          p("For details concerning the link functions, see the help for the R function",
            a(HTML(paste(code("brms::brmsfamily()"))),
              href = "https://paul-buerkner.github.io/brms/reference/brmsfamily.html",
              target = "_blank"),
            "and the", strong("brms"), "vignette",
            a("\"Parameterization of Response Distributions in brms\"",
              href = "https://CRAN.R-project.org/web/packages/brms/vignettes/brms_families.html",
              target = "_blank",
              .noWS = "after"),
            ". Note that for each parameter, the link function only applies if this parameter is",
            "actually modeled by (nonconstant) predictors. Currently, this is only supported",
            "for the location parameter (e.g.,", code("mu"), "for a Gaussian distribution)."),
          p("For details concerning the remaining (family-specific) parameters, see the help for the R function ",
            a(HTML(paste(code("brms::set_prior()"))),
              href = "https://paul-buerkner.github.io/brms/reference/set_prior.html",
              target = "_blank"),
            ".")
        )
      ),
      ### Predictors ------------------------------------------------------------
      tabPanel(
        "Predictors",
        titlePanel("Predictors"),
        br(),
        helpText(
          p("Choose the predictors (the independent variables). More specifically, you may define",
            "main effects of predictor variables and interactions between predictor variables.",
            "An overall intercept will always be included."),
          p("Numeric variables (with \"numeric\" including \"integer\") are treated as continuous",
            "predictor variables. Non-numeric variables are treated as nominal predictor variables.",
            "The type of a variable may be seen on page", actionLink("data_link2", "Data"),
            "when choosing the \"Structure\" preview type. If you want a numeric variable to be",
            "treated as a nominal predictor variable, you have to convert this variable in your",
            "dataset to a character variable, e.g., by changing the value", code("1"), "to",
            code("level1", .noWS = "after"), ", the value", code("2"), "to",
            code("level2", .noWS = "after"), ", and so on.",
            "For nominal predictor variables, the first level (after sorting alphabetically) will be the",
            "reference level.")
        ),
        wellPanel(
          h3("Main effects"),
          helpText(
            "Notes:",
            tags$ul(
              tags$li("Pooled effects are also known as",
                      em("population-level"), "or", em("fixed"), "effects."),
              tags$li("Partially pooled effects are also known as",
                      em("group-level"), "or", em("random"), "effects."),
            )
          ),
          h4("Pooled main effects"), # Abbreviated in the code by "CP" (for "completely pooled").
          helpText(
            "Start typing or click into the field below to choose variables for which",
            "pooled main effects shall be added."
          ),
          selectInput("pred_mainCP_sel", NULL,
                      choices = c("Choose variables for pooled main effects ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE),
          h4("Partially pooled main effects"),
          helpText(
            "Start typing or click into the field below to choose variables for which",
            "partially pooled main effects shall be added.",
            "Note that you may not specify partially pooled main effects for a numeric variable.",
            "This is not allowed to point out that a variable must be treated as nominal to have",
            "partially pooled main effects.",
            "If you really want partially pooled main effects for a numeric variable, you",
            "have to convert this variable in your dataset to a character variable."
          ),
          selectInput("pred_mainPP_sel", NULL,
                      choices = c("Choose variables for partially pooled main effects ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE)
        ),
        wellPanel(
          h3("Interaction effects"),
          helpText(
            p("Here, the term \"interaction\" not only denotes interactions involving only",
              "predictor variables with pooled effects (yielding an interaction with pooled effects),",
              "but also interactions involving predictor variables with partially pooled effects (yielding",
              "an interaction with partially pooled effects).",
              "This broad definition of \"interaction\" is indicated here by the symbol \"<-->\"."),
            p("Only variables already having a main effect may be included in an interaction",
              "term. In the rare case that you really need an interaction involving a variable",
              "without a main effect, you have to include this interaction manually as a",
              "variable in your dataset and add a main effect for this manually created",
              "interaction variable."),
            p("Start typing or click into the field below to choose variables for which an",
              "interaction term shall be added. Confirm this interaction term by pressing the",
              "\"Add interaction term\" button. All interaction terms which have been added are",
              "listed in the box below the \"Add interaction term\" button. You may edit this list",
              "to remove individual interaction terms or to re-add interaction terms which you",
              "have previously removed."),
          ),
          selectInput("pred_int_build", NULL,
                      choices = c("Choose variables for an interaction term ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE),
          actionButton("pred_int_add", "Add interaction term", class = "btn-primary"),
          br(),
          br(),
          selectInput("pred_int_sel", "Added interaction terms (you may edit this list, see above):",
                      choices = NULL,
                      multiple = TRUE,
                      selectize = TRUE)
        ),
        wellPanel(
          h3("Offsets"),
          helpText(
            p("An offset variable is a predictor variable with a coefficient fixed to 1.",
              "In most regression analyses, an offset is not needed.",
              "In the context of this app, the typical use case would be a count data outcome",
              "where the observation time differs from individual to individual (see the",
              strong("rstanarm"), "vignette",
              a("\"Estimating Generalized Linear Models for Count Data with rstanarm\"",
                href = "https://mc-stan.org/rstanarm/articles/count.html",
                target = "_blank"),
              "for an example)."),
            p("Start typing or click into the field below to choose variables for which",
              "offsets shall be added.")
          ),
          selectInput("offs_sel", NULL,
                      choices = c("Choose variables for offsets ..." = ""),
                      multiple = TRUE,
                      selectize = TRUE)
        ),
        wellPanel(
          h3("Preview of chosen predictor terms"),
          helpText(
            p("Here, you can get a preview of the currently chosen predictor terms.",
              "This is mainly intended as a check for those familiar with R's and",
              strong("brms", .noWS = "after"),
              "'s formula syntax. A preview of the full formula is given in the tab",
              HTML(paste(actionLink("formula_link1", "Formula preview")), .noWS = "after"), "."),
            p("A missing value (", code("NA", .noWS = "outside"), ") in column \"Group\" stands",
              "for the whole sample (i.e. no group). The value", code("1"), "in column \"Effect(s)\"",
              "stands for the intercept (or intercepts, if \"Group\" exists).")
          ),
          tableOutput("pred_view")
        )
      ),
      ### Formula preview -------------------------------------------------------
      tabPanel(
        "Formula preview",
        titlePanel("Formula preview"),
        br(),
        strong("Current formula:"),
        verbatimTextOutput("formula_view", placeholder = TRUE)
      ),
      id = "likelihood_navlist_ID"
    )
  ),
  
  ## Prior ------------------------------------------------------------------
  
  tabPanel(
    "Prior",
    titlePanel("Prior"),
    br(),
    helpText(HTML(paste0(
      "Build the joint prior distribution of all parameters in your model by ",
      "placing independent priors on all parameters separately. Notes:",
      tags$ul(
        tags$li("The default priors are taken from package", strong("brms", .noWS = "after"), "."),
        tags$li("For parameters for which you do not specify a custom prior, the default prior will be used."),
        tags$li("When specifying a custom prior, you may only choose a combination of",
                "\"Class\", \"Coefficient\", and \"Group\" which is also present in the",
                "table of the default priors."),
        tags$li("The names of the parameter classes are taken from", strong("brms"),
                "and may be translated as follows:",
                tags$ul(
                  tags$li(code("Intercept"), ": the intercept when centering the predictors.",
                          "This is only the internally used intercept; in the output, the intercept with",
                          "respect to the noncentered predictors is given (named", code("b_Intercept", .noWS = "after"), ")."),
                  tags$li(code("b"), ": pooled effects (or pooled regression coefficients)."),
                  tags$li(code("sd"), ": standard deviations of partially pooled effects."),
                  tags$li(code("cor"), ": correlations between partially pooled effects of the same group."),
                  tags$li("All other parameter classes are specific to the chosen",
                          "distributional family for the outcome (see page",
                          HTML(paste(actionLink("outcome_link1", HTML("Likelihood &rarr; Outcome"))), .noWS = "after"),
                          ").")
                )),
        tags$li("As soon as you choose a new dataset on page", actionLink("data_link3", "Data"), "(even if you upload",
                "the same dataset again), the custom priors are automatically reset."),
        tags$li("As soon as you change the likelihood, the custom priors are automatically reset.")
      ),
      "For details concerning the default priors, see the help for the R function ",
      a(HTML(paste(code("brms::set_prior()"))),
        href = "https://paul-buerkner.github.io/brms/reference/set_prior.html",
        target = "_blank"),
      ". For details concerning the specification of custom priors, see the help for ",
      a(HTML(paste(code("brms::set_prior()"))),
        href = "https://paul-buerkner.github.io/brms/reference/set_prior.html",
        target = "_blank"),
      " as well as the ",
      a(HTML("Stan documentation"),
        href = "https://mc-stan.org/users/documentation/",
        target = "_blank"),
      " (in particular, the ",
      a("\"Stan Functions Reference\"",
        href = "https://mc-stan.org/docs/2_21/functions-reference/index.html",
        target = "_blank"),
      ", here for Stan version 2.21.0 since this is the Stan version used by ",
      "the most recent version of ", strong("rstan"), "; for ",
      strong("cmdstanr"), ", the appropriate version depends on the installed ",
      "CmdStan version)."
    ))),
    hr(),
    h3("Default priors"),
    br(),
    strong("Default priors for the parameters belonging to the current likelihood:"),
    tableOutput("prior_default_view"),
    helpText("An empty field in column \"Prior\" denotes a flat prior over the support of the",
             "corresponding parameter."),
    hr(),
    h3("Custom priors"),
    br(),
    sidebarLayout(
      sidebarPanel(
        h4("Specification of a custom prior"),
        br(),
        selectInput("prior_class_sel",
                    HTML(paste0(
                      "Class:",
                      helpText("Note: This is the parameter class. It may consist of a single parameter.",
                               style = "font-weight:normal")
                    )),
                    choices = c("Choose class ..." = ""),
                    selectize = TRUE),
        selectInput("prior_coef_sel",
                    HTML(paste0(
                      "Coefficient:",
                      helpText("Note: Leave empty to use all coefficients belonging to the",
                               "selected class.",
                               style = "font-weight:normal")
                    )),
                    choices = c("Choose coefficient or leave empty" = ""),
                    selectize = TRUE),
        selectInput("prior_group_sel",
                    HTML(paste0(
                      "Group (for partially pooled effects):",
                      helpText("Note: Leave empty while having an empty \"Coefficient\" field to",
                               "use all groups belonging to the selected class.",
                               style = "font-weight:normal")
                    )),
                    choices = c("Choose group or leave empty" = ""),
                    selectize = TRUE),
        textInput("prior_text",
                  HTML(paste(
                    "Prior distribution:",
                    helpText(
                      HTML(paste(
                        "Note: You may", em("either"),
                        tags$ul(
                          tags$li(HTML(paste(
                            "specify a prior distribution using a Stan function (see the",
                            a("\"Stan Functions Reference\"",
                              href = "https://mc-stan.org/docs/2_21/functions-reference/index.html",
                              target = "_blank"),
                            "for details),", em("or")
                          ))),
                          tags$li(HTML(paste(
                            "specify a prior distribution using one of the",
                            "special (pseudo-)functions defined by", strong("brms"),
                            "for this purpose (see",
                            a(HTML(paste(code("brms::set_prior()"))),
                              href = "https://paul-buerkner.github.io/brms/reference/set_prior.html",
                              target = "_blank"),
                            "for details; an important example is", code("lkj"),
                            "for parameters of class", code("cor", .noWS = "after"), "),", em("or")
                          ))),
                          tags$li("leave this field empty to use a flat prior.")
                        ),
                        "If you specify a prior distribution using a Stan function, you have to",
                        "use the Stan function which would be used in a Stan sampling statement",
                        "and specify values for all arguments of this Stan function (e.g.,",
                        code("normal(0, 2.5)", .noWS = "after"), ")."
                      )),
                      style = "font-weight:normal"
                    )
                  )),
                  placeholder = "Enter prior distribution using a Stan function or leave empty to use a flat prior"),
        actionButton("prior_add", "Add prior", class = "btn-primary"),
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
        helpText("An empty field in column \"Prior\" denotes a flat prior over the support of the",
                 "corresponding parameter.")
      )
    )
  ),
  
  ## Posterior --------------------------------------------------------------
  
  tabPanel(
    "Posterior",
    titlePanel("Posterior"),
    br(),
    helpText(
      "Use", a("Stan", href = "https://mc-stan.org/", target = "_blank"),
      "to infer the joint posterior distribution of all parameters in your model",
      "by sampling. More specifically, Stan uses a modified",
      # "variant of the",
      "no-U-turn sampler (NUTS)",
      # a("(Hoffman and Gelman, 2014)",
      #   href = "http://jmlr.org/papers/v15/hoffman14a.html",
      #   target = "_blank"),
      "which is a special Hamiltonian Monte Carlo (HMC)",
      "sampler which in turn is a special Markov chain Monte Carlo (MCMC) sampler.",
      "Details concerning Stan's sampling algorithm may be found in the",
      a("Stan documentation",
        href = "https://mc-stan.org/users/documentation/",
        target = "_blank",
        .noWS = "after"),
      "."
    ),
    navlistPanel(
      ### Run Stan --------------------------------------------------------------
      tabPanel(
        "Run Stan",
        titlePanel("Run Stan"),
        br(),
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
            verbatimTextOutput("stancode_view", placeholder = TRUE)
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
          helpText(
            "Here, you can set advanced options for the R function",
            a(HTML(paste(code("brms::brm()"))),
              href = "https://paul-buerkner.github.io/brms/reference/brm.html",
              target = "_blank"),
            "which is the central function for inferring the posterior. These",
            "advanced options have sensible defaults, but sometimes, they need",
            "to be changed."
          ),
          checkboxInput("show_advOpts", "Show advanced options", value = FALSE),
          conditionalPanel(
            condition = "input.show_advOpts",
            helpText(
              p("For most of the following advanced options, details may be",
                "found on the",
                a(HTML(paste(code("brms::brm()"))),
                  href = "https://paul-buerkner.github.io/brms/reference/brm.html",
                  target = "_blank"),
                "help page. However, there are also some backend-specific",
                "advanced options for which the following help pages need to",
                "be consulted:",
                tags$ul(
                  tags$li(
                    "For the",
                    a(HTML(paste(strong("rstan"))),
                      href = "https://mc-stan.org/rstan/",
                      target = "_blank"),
                    "backend:",
                    a(HTML(paste(code("rstan::sampling()"))),
                      href = "https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html",
                      target = "_blank", .noWS = "after"),
                    ", together with",
                    a(HTML(paste(code("rstan::stan()"))),
                      href = "https://mc-stan.org/rstan/reference/stan.html",
                      target = "_blank", .noWS = "after"),
                    "."
                  ),
                  tags$li(
                    "For the",
                    a(HTML(paste(strong("cmdstanr"))),
                      href = "https://mc-stan.org/cmdstanr/",
                      target = "_blank"),
                    "backend:",
                    a(HTML(paste(code("$sample()"))),
                      href = "https://mc-stan.org/cmdstanr/reference/model-method-sample.html",
                      target = "_blank", .noWS = "after"),
                    ", together with the",
                    a("\"CmdStan User's Guide\"",
                      href = "https://mc-stan.org/docs/2_28/cmdstan-guide/index.html",
                      target = "_blank", .noWS = "after"),
                    "."
                  )
                )),
              p("Notes:",
                tags$ul(
                  tags$li(
                    "Numeric options with a preset value may not be left empty."
                  ),
                  tags$li(
                    "If unset, option \"Seed\" internally defaults to a random",
                    "seed, giving nonreproducible results. To obtain reproducible",
                    "results, you need to specify a value for option \"Seed\"",
                    "and enter this value each time you want to obtain the same",
                    "results again."
                  ),
                  tags$li(
                    "Internally, the value supplied to option \"Cores\" is cut",
                    "off at the value supplied to option \"Chains\"."
                  ),
                  tags$li(
                    "If unset, option \"Warmup iterations per chain\" internally",
                    "defaults to half of option \"Total iterations per chain\"",
                    "(rounded down if this fraction is not an integer)."
                  ),
                  tags$li(
                    "If unset, option \"Progress-refreshing step size\" internally",
                    "defaults to a tenth of option \"Total iterations per chain\",",
                    "but at least 1."
                  ),
                  tags$li(
                    "If unset, option \"Range of random initial values in the",
                    "unconstrained parameter space\" internally defaults to 2."
                  )
                ))
            ),
            fluidRow(
              column(5,
                     radioButtons("advOpts_backend", "Backend:",
                                  choiceNames = list(strong("rstan"), strong("cmdstanr")),
                                  choiceValues = list("rstan", "cmdstanr"),
                                  selected = getOption("brms.backend", "rstan"),
                                  inline = TRUE),
                     numericInput("advOpts_seed", "Seed:",
                                  value = NA, step = 1L),
                     numericInput("advOpts_cores", "Cores:",
                                  value = getOption("mc.cores", parallel::detectCores(logical = FALSE)),
                                  step = 1L, min = 1L),
                     numericInput("advOpts_chains", "Chains (MCMC chains):",
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
                     numericInput("advOpts_init_r",
                                  HTML(paste0(
                                    "Range of random initial values in the ",
                                    "unconstrained parameter space (",
                                    code("init_r"), " in ", strong("rstan"), ", ",
                                    code("init"), " in ", strong("cmdstanr"),
                                    "; only relevant if random initial values ",
                                    "are chosen):"
                                  )),
                                  value = NA, step = 0.1, min = 0),
                     numericInput("advOpts_adapt_delta",
                                  HTML(paste0("Target Metropolis acceptance rate (", code("adapt_delta"), "):")),
                                  value = 0.95, step = 0.01, min = 0, max = 1),
                     numericInput("advOpts_max_treedepth",
                                  HTML(paste0("Maximum tree depth (", code("max_treedepth"), "):")),
                                  value = 15L, step = 1L, min = 1L),
                     checkboxInput("advOpts_open_progress", strong("Open progress"),
                                   value = TRUE),
                     numericInput("advOpts_refresh",
                                  HTML(paste0("Progress-refreshing step size (", code("refresh"), "):")),
                                  value = NA, step = 1L, min = 0L),
                     checkboxInput("advOpts_save_all_pars",
                                   strong("Save draws for", em("all"), "parameters, including internal ones"),
                                   value = FALSE),
                     checkboxInput("advOpts_save_warmup", strong("Save warmup"),
                                   value = TRUE))
            )
          )
        ),
        wellPanel(
          h3("Run Stan"),
          helpText(
            p("Start the Stan run for inferring the posterior here (or upload",
              "the results from a previous Stan run instead)."),
            p("Notes:",
              tags$ul(
                tags$li(
                  "If the advanced option \"Open progress\" is selected (as",
                  "per default), Windows users having Firefox set as their default",
                  "web browser may need to manually copy the link to the Stan HTML",
                  "progress file which is automatically opening up and paste this",
                  "link into a different web browser for viewing the progress file",
                  "there."
                ),
                tags$li(
                  "In general, uploading the results from a previous Stan run",
                  "will cause a mismatch between the content shown on page",
                  HTML(paste(actionLink("posterior_link_upld", "Posterior"))),
                  "versus the content shown on pages",
                  HTML(paste(actionLink("likelihood_link_upld", "Likelihood"))),
                  "and",
                  HTML(paste(actionLink("prior_link_upld", "Prior")), .noWS = "after"),
                  "."
                ),
                tags$li(
                  "If uploaded Stan results are used, then", strong("shinybrms"),
                  "currently cannot check whether the number of chains in the",
                  "Stan results differs from the desired number of chains",
                  "(i.e., from the number of chains specified originally)."
                )
              ))
          ),
          actionButton("run_stan", "Run Stan (may take a while)", class = "btn-primary"),
          br(),
          br(),
          uiOutput("brmsfit_upload_UI"),
          hr(),
          strong("Date and time when the Stan run was finished:"),
          verbatimTextOutput("fit_date", placeholder = TRUE),
          strong("Important software versions used for this Stan run:"),
          verbatimTextOutput("fit_version", placeholder = TRUE),
          strong("Check if all MCMC diagnostics are OK (see the tab",
                 actionLink("mcmc_link1", "MCMC diagnostics"),
                 "for details):"),
          verbatimTextOutput("diagn_all_out", placeholder = TRUE),
          selectInput("stanout_download_sel", "Choose output file to download:",
                      choices = c("\"brmsfit\" object (RDS file)" = "shinybrms_brmsfit.rds",
                                  "Matrix of posterior draws (CSV file)" = "shinybrms_post_draws_mat.csv",
                                  "Matrix of posterior draws (RDS file)" = "shinybrms_post_draws_mat.rds",
                                  "Array of posterior draws (RDS file)" = "shinybrms_post_draws_arr.rds"),
                      width = "320px",
                      selectize = TRUE),
          helpText(
            "The most comprehensive output object is the", code("brmsfit"),
            "object which is the output from the R function",
            a(HTML(paste(code("brms::brm()"))),
              href = "https://paul-buerkner.github.io/brms/reference/brm.html",
              target = "_blank", .noWS = "after"),
            ", the central function for inferring the posterior. Such a",
            code("brmsfit"), "object may be uploaded later above to avoid",
            "running Stan (for", em("that"), "model and", em("that"), "data)",
            "again."
          ),
          downloadButton("stanout_download", "Download output file")
        )
      ),
      ### MCMC diagnostics ------------------------------------------------------
      tabPanel(
        "MCMC diagnostics",
        titlePanel("MCMC diagnostics"),
        br(),
        helpText(withMathJax(
          p("Before using the Stan results for posterior inference, it is important to check the",
            "convergence and efficiency of the sampling algorithm for the given model and data by",
            "the help of", em("MCMC diagnostics", .noWS = "after"), ".",
            "The MCMC diagnostics used here may be divided into",
            em("HMC-specific diagnostics"), "and", em("general MCMC diagnostics", .noWS = "after"), ".",
            "Lists of these two groups of MCMC diagnostics may be found below, together with some",
            "basic guidelines for their interpretation. These basic guidelines are also checked",
            "automatically. Note that these are", em("basic"), "guidelines", HTML("&ndash;"),
            "false positive and false negative alarms are possible and",
            "in some situations, false alarms are more likely than in others.",
            "For details concerning the MCMC diagnostics used here, see the",
            a("\"Brief Guide to Stan's Warnings\"",
              href = "https://mc-stan.org/misc/warnings.html",
              target = "_blank",
              .noWS = "after"),
            ",",
            a("Betancourt (2018)",
              href = "https://arxiv.org/abs/1701.02434v2",
              target = "_blank",
              .noWS = "after"),
            ", and",
            a("Vehtari et al. (2021)",
              href = "https://doi.org/10.1214/20-BA1221",
              target = "_blank",
              .noWS = "after"),
            ". The \"Brief Guide to Stan's Warnings\" covers all MCMC diagnostics used here and",
            "gives some advice on what to do if they indicate problems.",
            "Betancourt (2018) focuses on the HMC-specific diagnostics,",
            "whereas Vehtari et al. (2021) focus on the general MCMC diagnostics."),
          p("The HMC-specific diagnostics are:",
            tags$ul(
              tags$li("the number of iterations ending with a divergence,"),
              tags$li("the number of iterations hitting the maximum tree depth,"),
              tags$li("the Bayesian fraction of missing information for the energy transitions (E-BFMI) of each chain.")
            ),
            "In general, the first two of these diagnostics are worrying if they are greater than",
            "zero (i.e. at least one iteration ending with a divergence or at least one iteration",
            "hitting the maximum tree depth) and the third diagnostic (E-BFMI) is worrying if it is",
            "smaller than 0.2 for at least one chain."),
          p("The general MCMC diagnostics (computed for each parameter",
            "as well as for the accumulated log-posterior density) are:",
            tags$ul(
              tags$li(
                "the modified \\(\\widehat{R}\\)",
                "proposed by Vehtari et al. (2021)",
                "(here simply called", em("the"), "\\(\\widehat{R}\\) instead of",
                em("the modified"), "\\(\\widehat{R}\\)),"
              ),
              tags$li(
                "the effective sample size (ESS) in the bulk of the corresponding marginal posterior",
                "distribution (short: bulk-ESS or \\(\\text{ESS}_{\\text{bulk}}\\)),"
              ),
              tags$li(
                "the ESS in the tails of the corresponding marginal posterior",
                "distribution (short: tail-ESS or \\(\\text{ESS}_{\\text{tail}}\\))."
              )
            ),
            "In general, the following values of the general MCMC diagnostics are worrying:",
            tags$ul(
              tags$li(
                "\\(\\widehat{R} \\geq 1.01\\),"
              ),
              tags$li(
                "\\(\\text{ESS}_{\\text{bulk}} \\leq 100 \\cdot n_{\\text{chains}}\\)",
                "with \\(n_{\\text{chains}}\\) denoting the number of chains,"
              ),
              tags$li(
                "\\(\\text{ESS}_{\\text{tail}} \\leq 100 \\cdot n_{\\text{chains}}\\)."
              )
            )),
          p("Note: If you used a", code("constant()"), "prior (which should rarely be the case),",
            "then after obtaining the Stan results, you will be warned that at least one MCMC diagnostic is worrying.",
            "The reason is that",
            "\\(\\widehat{R}\\), \\(\\text{ESS}_{\\text{bulk}}\\), and \\(\\text{ESS}_{\\text{tail}}\\)",
            "cannot be calculated for a constant parameter. Thus, with respect to",
            "the \\(\\widehat{R}\\), \\(\\text{ESS}_{\\text{bulk}}\\), and \\(\\text{ESS}_{\\text{tail}}\\)",
            em("of a constant parameter", .noWS = "after"), ", you may ignore the warning.",
            "However, the MCMC diagnostics may be worrying for other reasons or other parameters as well.",
            "Thus, in this case, you need to check the MCMC diagnostics very carefully.",
            "In particular, you need to check the HMC-specific diagnostics as well as",
            "the detailed table of the general MCMC diagnostics.")
          # (where a constant parameter has only missing values, i.e., only",
          # code("NA", .noWS = "after"), "s)
        )),
        br(),
        wellPanel(
          h3("HMC-specific diagnostics"),
          strong("Divergences:"),
          verbatimTextOutput("diagn_div_out", placeholder = TRUE),
          strong("Hits of maximum tree depth:"),
          verbatimTextOutput("diagn_tree_out", placeholder = TRUE),
          strong("E-BFMI:"),
          verbatimTextOutput("diagn_EBFMI_out", placeholder = TRUE),
        ),
        wellPanel(
          h3("General MCMC diagnostics"),
          strong(withMathJax("\\(\\widehat{R}\\):")),
          verbatimTextOutput("rhat_out", placeholder = TRUE),
          strong("Bulk-ESS:"), # strong(withMathJax("\\(\\text{ESS}_{\\text{bulk}}\\):")),
          verbatimTextOutput("essBulk_out", placeholder = TRUE),
          strong("Tail-ESS:"), # strong(withMathJax("\\(\\text{ESS}_{\\text{tail}}\\):")),
          verbatimTextOutput("essTail_out", placeholder = TRUE),
          checkboxInput("show_general_MCMC_tab",
                        "Show detailed table of the general MCMC diagnostics",
                        value = FALSE),
          conditionalPanel(
            condition = "input.show_general_MCMC_tab",
            verbatimTextOutput("general_MCMC_out", placeholder = TRUE)
          )
        ),
        downloadButton("diagn_download", "Download list of MCMC diagnostics (RDS file)"),
        br(),
        br()
      ),
      ### Default summary -------------------------------------------------------
      tabPanel(
        "Default summary",
        titlePanel("Default summary"),
        br(),
        helpText("Notes:",
                 tags$ul(
                   tags$li("Column", code("Estimate"), "contains the posterior median."),
                   tags$li("Column", code("Est.Error"), "contains the posterior median absolute deviation."),
                   tags$li("Column", code("l-95% CI"), "contains the lower boundary of the 95% central posterior interval."),
                   tags$li("Column", code("u-95% CI"), "contains the upper boundary of the 95% central posterior interval.")
                 )),
        br(),
        verbatimTextOutput("smmry_view", placeholder = TRUE),
        downloadButton("smmry_download", "Download default summary"),
        br(),
        br()
      ),
      ### Custom summary --------------------------------------------------------
      tabPanel(
        "Custom summary",
        titlePanel("Custom summary"),
        br(),
        helpText(p("Here, you may calculate posterior summary quantities for a custom mathematical",
                   "(or logical) expression involving at least one parameter.",
                   "Click", HTML(paste(actionLink("cust_allow_link", "here"))),
                   "for a list of characters and character groups which are allowed in the custom expression.",
                   "For details on how to use these characters or character groups, see the examples below",
                   "or", HTML(paste(actionLink("cust_help_link", "these links")), .noWS = "after"), ".",
                   "Parameter names need to be enclosed in backticks (", code("`", .noWS = "outside"), ").",
                   "The drop-down list below may be used for inserting parameter names (directly with",
                   "enclosing backticks) into the custom expression."),
                 p("Fictitious examples for a custom expression would be:",
                   tags$ul(
                     tags$li(code("`b_age` + `b_age:genderM`")),
                     tags$li(code("log(`sigma`)")),
                     tags$li(code("`b_treatment` > 0.2"))
                   ),
                   "For the latter example, the posterior mean gives the posterior probability that",
                   code("`b_treatment` > 0.2", .noWS = "after"),
                   ".")),
        br(),
        textInput("cust_text", "Custom expression involving at least one parameter:",
                  placeholder = "Enter expression ...",
                  width = "100%"),
        selectInput("par_sel", "Parameter name to insert:",
                    choices = c("Choose parameter name ..." = ""),
                    selectize = TRUE),
        actionButton("par_add", "Insert parameter name"),
        br(),
        br(),
        textInput("cust_name", "Name for the custom expression (optional):",
                  placeholder = "Enter name or leave empty"),
        br(),
        actionButton("cust_act", "Calculate posterior summary quantities", class = "btn-primary"),
        br(),
        br(),
        strong("Posterior summary quantities:"),
        tableOutput("cust_view"),
        helpText("Note: All columns contain", em("posterior"), "summary quantities.",
                 "In particular, the columns starting with \"Q\" contain the corresponding",
                 "posterior percentiles and column \"MAD\" contains the posterior median absolute deviation."),
        downloadButton("cust_smmry_download", "Download custom summary"),
        br(),
        br()
      ),
      ### Conditional effects ---------------------------------------------------
      tabPanel(
        "Conditional effects",
        titlePanel("Conditional effects"),
        br(),
        helpText(
          p("A conditional-effects plot shows the estimated effect of a predictor variable on the outcome.",
            "An interaction effect involving at most two predictor variables may also be visualized",
            "by showing the estimated effect of the first predictor variable (involved in this interaction)",
            "separately for appropriate values",
            # Thereby, "appropriate" means: "at the mean" as well as at "mean
            # plus/minus one standard deviation" for continuous predictor
            # variables and at all categories for categorical predictor
            # variables.
            "of the second predictor variable (involved in this interaction)."),
          p("As its name suggests, a conditional-effects plot", em("conditions"), "on specific values of",
            "those predictor variables which are not involved in the plot:",
            "It conditions on the mean of continuous predictor variables and",
            "on the reference category of those categorical predictor variables which have pooled main effects.",
            "Partially pooled effects are set to zero, with the following exceptions:",
            tags$ul(
              tags$li(
                "Those partially pooled effects which are plotted are not set to zero (otherwise,",
                "there would not be anything meaningful to plot)."
              ),
              tags$li(
                "If partially pooled slopes are plotted, the corresponding partially pooled intercepts",
                "are also not set to zero (for consistency with pooled interaction effects)."
                # More precisely: "for consistency with pooled interaction
                # effects (and this also avoids problems with dummy-coded
                # partially pooled slopes)"
              ) 
            )),
          p("Be cautious with predictor variables having a high number of levels (which is usually",
            "only the case for partially pooled effects): In that case, the computation may",
            "take a long time and the resulting plot is rarely useful.")
        ),
        # br(),
        selectInput("term_sel", "Predictor term to plot:",
                    choices = c("Choose predictor term ..." = ""),
                    selectize = TRUE),
        # br(),
        ### Only for getting the width in pixels corresponding to argument
        ### 'width = "100%"'.
        plotOutput("size_aux", width = "100%", height = "1px"),
        ### 
        plotOutput("ceff_plot", inline = TRUE),
        br(),
        selectInput("ceff_download_sel", "Choose file format for download:",
                    choices = c("PDF" = "pdf",
                                "JPEG" = "jpeg",
                                "PNG" = "png",
                                "BMP" = "bmp",
                                "TIFF" = "tiff",
                                "SVG" = "svg"),
                    selectize = TRUE),
        helpText(
          "If you want to download the plot in a different size, simply adjust your browser window size",
          "until the plot in the app has the desired size and then download the plot."
        ),
        downloadButton("ceff_download", "Download plot"),
        br(),
        br()
      ),
      ### Launch **shinystan** --------------------------------------------------
      tabPanel(
        HTML(paste("Launch", strong("shinystan"))),
        titlePanel(HTML(paste("Launch", strong("shinystan")))),
        br(),
        helpText(
          p("The package",
            a(HTML(paste(strong("shinystan"))), href = "https://mc-stan.org/shinystan/", target = "_blank"),
            "provides a", strong("shiny"), "app offering an interactive inspection of Stan's output."),
          p("Notes:",
            tags$ul(
              tags$li(
                "In the", strong("shinystan"), "app, the parameter names given by", strong("brms"),
                "are used. These are closely related to the parameter classes listed on page",
                HTML(paste(actionLink("prior_link3", "Prior"))), "and may be",
                "summarized as follows:",
                tags$ul(
                  tags$li(code("b_Intercept"), "is the intercept (with respect to the noncentered predictors)."),
                  tags$li("The parameters starting with", code("b_"), "are the pooled effects."),
                  tags$li("If you used a", code("constant()"), "prior (which should rarely be the case), then",
                          "the parameters starting with", code("par_"), "are internal parameters which you don't",
                          "need to take into account."),
                  tags$li("The parameters starting with", code("r_"), "are the partially pooled effects."),
                  tags$li("The parameters starting with", code("sd_"), "are the standard deviations of the",
                          "partially pooled effects."),
                  tags$li("The parameters starting with", code("cor_"), "are the correlations between the",
                          "partially pooled effects of the same group."),
                  tags$li(code("log-posterior"), "is the accumulated log-posterior density (up to an additive constant)."),
                  tags$li("All other parameters are parameters specific to the chosen",
                          "distributional family for the outcome (see page",
                          HTML(paste(actionLink("outcome_link2", HTML("Likelihood &rarr; Outcome"))), .noWS = "after"),
                          ").")
                )
              ),
              tags$li(
                HTML(paste0(
                  "The R objects needed for the posterior predictive checks in ", strong("shinystan"),
                  " are automatically created. These are the observations for the outcome (object ",
                  code("y"), ") and the corresponding posterior predictive replications (object ",
                  code("y_rep"), "). You can select them in the respective \"Object from global ",
                  "environment\" input selector under \"DIAGNOSE\" &rarr; \"PPcheck\" &rarr; ",
                  "\"Select data\" in the ", strong("shinystan"), " app."
                ))
              )
            )
          )
        ),
        numericInput("seed_PPD",
                     paste("Seed for draws from the posterior predictive distribution",
                           "(leave empty to use a random seed):"),
                     value = NA, step = 1L),
        actionButton("act_launch_shinystan",
                     HTML(paste("Launch", strong("shinystan"), "(may take a while)")),
                     class = "btn-primary")
      ),
      id = "posterior_navlist_ID"
    )
  ),
  
  ## More -------------------------------------------------------------------
  
  navbarMenu(
    "More",
    ### About -----------------------------------------------------------------
    tabPanel(
      "About",
      titlePanel(HTML(paste("About", strong("shinybrms")))),
      br(),
      wellPanel(
        h3("Basic information"),
        tags$ul(
          tags$li(strong("Author:"),
                  "Frank Weber (ORCID iD:",
                  a("0000-0002-4842-7922",
                    href = "https://orcid.org/0000-0002-4842-7922",
                    target = "_blank",
                    .noWS = "after"),
                  ", e-mail address:",
                  a("fweber144@protonmail.com",
                    href = "mailto:fweber144@protonmail.com",
                    .noWS = "after"),
                  ")"),
          tags$li(strong("Contributors:"),
                  "Thomas Park (for the Bootswatch theme \"United\");",
                  "Twitter, Inc. (for Bootstrap, the basis for the Bootswatch theme \"United\");",
                  "Google, LLC (for the \"Open Sans\" font)"),
          tags$li(strong("Version:"),
                  "1.6.0"),
          tags$li(strong("Date:"),
                  "November 04, 2021"),
          tags$li(strong("Citation:"),
                  "Frank Weber (2021).",
                  em("shinybrms: Graphical User Interface ('shiny' App)",
                     "for 'brms'."),
                  "R package, version 1.6.0. URL:",
                  a("https://fweber144.github.io/shinybrms/",
                    href = "https://fweber144.github.io/shinybrms/",
                    target = "_blank",
                    .noWS = "after"),
                  "."),
          tags$li(strong("Links:"),
                  a("website", href = "https://fweber144.github.io/shinybrms/", target = "_blank", .noWS = "after"), ",",
                  a("CRAN", href = "https://CRAN.R-project.org/package=shinybrms", target = "_blank", .noWS = "after"), ",",
                  a("GitHub", href = "https://github.com/fweber144/shinybrms", target = "_blank"))
        )
      ),
      wellPanel(
        h3("Issues"),
        "If you need help, if you want to suggest improvements, or if you found",
        "a bug, please open an issue on",
        a("GitHub", href = "https://github.com/fweber144/shinybrms/issues", target = "_blank", .noWS = "after"),
        "."
      ),
      wellPanel(
        h3("License information"),
        "The", strong("shinybrms"), "package as a whole is distributed under the",
        a("GPL-3",
          href = "https://CRAN.R-project.org/web/licenses/GPL-3",
          target = "_blank",
          .noWS = "after"),
        ". However, the", strong("shinybrms"), "package includes other open source software components.",
        "A list of these components (together with the full copies of the license agreements",
        "used by these components) may be found",
        a("here (for the latest CRAN version)",
          href = "https://CRAN.R-project.org/web/packages/shinybrms/LICENSE",
          target = "_blank"),
        "and",
        a("here (for the latest GitHub version)",
          href = "https://github.com/fweber144/shinybrms/blob/master/LICENSE",
          target = "_blank",
          .noWS = "after"),
        "."
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
          tags$li("Windows is a registered trademark of Microsoft Corporation in the United States and other countries."),
          tags$li("Firefox is a trademark of the Mozilla Foundation in the U.S. and other countries.")
        )
      )
    ),
    ### Links -----------------------------------------------------------------
    tabPanel(
      "Links",
      titlePanel("Links"),
      br(),
      wellPanel(
        h3("Programming languages and software environments"),
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
            a("website", href = "https://fweber144.github.io/shinybrms/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=shinybrms", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/fweber144/shinybrms", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("brms"), ": ",
            a("website", href = "https://paul-buerkner.github.io/brms/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=brms", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/paul-buerkner/brms", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("rstan"), ": ",
            a("website", href = "https://mc-stan.org/rstan/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=rstan", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/stan-dev/rstan/", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("cmdstanr"), ": ",
            a("website", href = "https://mc-stan.org/cmdstanr/", target = "_blank"), ", ",
            # a("CRAN", href = "https://CRAN.R-project.org/package=cmdstanr", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/stan-dev/cmdstanr/", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("shinystan"), ": ",
            a("website", href = "https://mc-stan.org/shinystan/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=shinystan", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/stan-dev/shinystan/", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("lme4"), ": ",
            a("CRAN", href = "https://CRAN.R-project.org/package=lme4", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/lme4/lme4/", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("MASS"), ": ",
            a("CRAN", href = "https://CRAN.R-project.org/package=MASS", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("rstanarm"), ": ",
            a("website", href = "https://mc-stan.org/rstanarm/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=rstanarm", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/stan-dev/rstanarm/", target = "_blank")
          ))),
          tags$li(HTML(paste0(
            strong("shiny"), ": ",
            a("website", href = "https://shiny.rstudio.com/", target = "_blank"), ", ",
            a("CRAN", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), ", ",
            a("GitHub", href = "https://github.com/rstudio/shiny", target = "_blank")
          )))
        )
      )
    ),
    ### References ------------------------------------------------------------
    tabPanel(
      "References",
      titlePanel("References"),
      br(),
      p("Betancourt M (2018).",
        "A conceptual introduction to Hamiltonian Monte Carlo.",
        em("arXiv:170102434v2 [stat]", .noWS = "after"), ". URL:",
        a("https://arxiv.org/abs/1701.02434v2",
          href = "https://arxiv.org/abs/1701.02434v2",
          target = "_blank"),
        "(visited on July 26, 2020)."),
      p("Gelman A, Carlin JB, Stern HS, Dunson DB, Vehtari A, and Rubin DB (2014).",
        em("Bayesian Data Analysis", .noWS = "after"),
        ". 3rd ed. Boca Raton, FL: CRC Press."),
      p("McElreath R (2020).",
        em("Statistical Rethinking: A Bayesian Course with Examples in R and Stan", .noWS = "after"),
        ". 2nd ed. Boca Raton, FL: CRC Press."),
      p(withMathJax(
        "Vehtari A, Gelman A, Simpson D, Carpenter B, and B", HTML("&uuml;", .noWS = "outside"), "rkner P-C (2021).",
        "Rank-normalization, folding, and localization: An improved \\(\\widehat{R}\\) for",
        "assessing convergence of MCMC.", em("Bayesian Analysis", .noWS = "after"), ",",
        em("-1", .noWS = "after"), "(-1), 1", HTML("&#8212;", .noWS = "outside"), "28. DOI:",
        a("10.1214/20-BA1221", href = "https://doi.org/10.1214/20-BA1221", target = "_blank",
          .noWS = "after"),
        "."))
    )
  ),
  
  ## Quit -------------------------------------------------------------------
  
  tabPanel(title = "Quit", value = "quit_app", icon = icon("power-off")),
  theme = "united_mod.min.css"
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## Links ------------------------------------------------------------------
  
  observeEvent({
    input$data_link1
    input$data_link2
    input$data_link3
  }, {
    updateNavbarPage(session, "navbar_ID", "Data")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent({
    input$likelihood_link1
    input$likelihood_link_upld
  }, {
    updateNavbarPage(session, "navbar_ID", "Likelihood")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent({
    input$outcome_link1
    input$outcome_link2
  }, {
    updateNavbarPage(session, "navbar_ID", "Likelihood")
    updateNavlistPanel(session, "likelihood_navlist_ID", "Outcome")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$formula_link1, {
    updateNavbarPage(session, "navbar_ID", "Likelihood")
    updateNavlistPanel(session, "likelihood_navlist_ID", "Formula preview")
  })
  
  observeEvent({
    input$prior_link1
    input$prior_link3
    input$prior_link_upld
  }, {
    updateNavbarPage(session, "navbar_ID", "Prior")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent({
    input$posterior_link1
    input$posterior_link_upld
  }, {
    updateNavbarPage(session, "navbar_ID", "Posterior")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$mcmc_link1, {
    updateNavbarPage(session, "navbar_ID", "Posterior")
    updateNavlistPanel(session, "posterior_navlist_ID", "MCMC diagnostics")
  })
  
  observeEvent(input$about_link1, {
    updateNavbarPage(session, "navbar_ID", "About")
  })
  
  observeEvent(input$links_link1, {
    updateNavbarPage(session, "navbar_ID", "Links")
  })
  
  observeEvent(input$references_link1, {
    updateNavbarPage(session, "navbar_ID", "References")
  })
  
  ## Data -------------------------------------------------------------------
  
  da <- reactive({
    if (identical(input$ex_da_sel, "Arabidopsis")) {
      if (requireNamespace("lme4", quietly = TRUE)) {
        tmp_env <- new.env()
        data(Arabidopsis, package = "lme4", envir = tmp_env)
        assign("Arabidopsis", within(get("Arabidopsis", envir = tmp_env), {
          gen <- as.factor(gen)
          rack <- as.factor(rack)
          nutrient <- as.factor(nutrient)
        }), envir = tmp_env)
        da_tmp <- get("Arabidopsis", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("lme4"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "bacteria")) {
      if (requireNamespace("MASS", quietly = TRUE)) {
        tmp_env <- new.env()
        data(bacteria, package = "MASS", envir = tmp_env)
        da_tmp <- get("bacteria", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("MASS"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "birthwt")) {
      if (requireNamespace("MASS", quietly = TRUE)) {
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
        da_tmp <- get("birthwt", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("MASS"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "epilepsy")) {
      if (requireNamespace("brms", quietly = TRUE)) {
        tmp_env <- new.env()
        data(epilepsy, package = "brms", envir = tmp_env)
        da_tmp <- get("epilepsy", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("brms"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "grouseticks")) {
      if (requireNamespace("lme4", quietly = TRUE)) {
        tmp_env <- new.env()
        data(grouseticks, package = "lme4", envir = tmp_env)
        da_tmp <- get("grouseticks", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("lme4"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "kidiq")) {
      if (requireNamespace("rstanarm", quietly = TRUE)) {
        tmp_env <- new.env()
        data(kidiq, package = "rstanarm", envir = tmp_env)
        assign("kidiq", within(get("kidiq", envir = tmp_env), {
          mom_hs <- factor(paste0("hs", mom_hs))
        }), envir = tmp_env)
        da_tmp <- get("kidiq", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("rstanarm"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "Puromycin")) {
      da_tmp <- Puromycin
    } else if (identical(input$ex_da_sel, "quine")) {
      if (requireNamespace("MASS", quietly = TRUE)) {
        tmp_env <- new.env()
        data(quine, package = "MASS", envir = tmp_env)
        da_tmp <- get("quine", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("MASS"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "Rabbit")) {
      if (requireNamespace("MASS", quietly = TRUE)) {
        tmp_env <- new.env()
        data(Rabbit, package = "MASS", envir = tmp_env)
        da_tmp <- get("Rabbit", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("MASS"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "roaches")) {
      if (requireNamespace("rstanarm", quietly = TRUE)) {
        tmp_env <- new.env()
        data(roaches, package = "rstanarm", envir = tmp_env)
        assign("roaches", within(get("roaches", envir = tmp_env), {
          # Code from <https://avehtari.github.io/modelselection/roaches.html>
          # and <https://mc-stan.org/rstanarm/articles/count.html>, but slightly
          # modified:
          exposure2_log <- log(exposure2)
          roach1_scaledBy0.01 <- 0.01 * roach1
          roach1_sqrt <- sqrt(roach1)
        }), envir = tmp_env)
        da_tmp <- get("roaches", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("rstanarm"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "sleepstudy")) {
      if (requireNamespace("lme4", quietly = TRUE)) {
        tmp_env <- new.env()
        data(sleepstudy, package = "lme4", envir = tmp_env)
        da_tmp <- get("sleepstudy", envir = tmp_env)
      } else {
        showNotification(
          HTML(paste("Package", strong("lme4"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    } else if (identical(input$ex_da_sel, "ToothGrowth")) {
      da_tmp <- ToothGrowth
    } else {
      req(input$data_upload)
      da_tmp <- try(read.csv(input$data_upload$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote,
                             dec = input$dec,
                             na.strings = c("NA", ".")),
                    silent = TRUE)
      if (inherits(da_tmp, "try-error")) {
        showModal(modalDialog(
          "The file upload failed.",
          title = "File upload failed",
          footer = modalButton("Close"),
          size = "s",
          easyClose = TRUE
        ))
        req(FALSE)
      }
      if ("." %in% names(da_tmp)) {
        if (!"X." %in% names(da_tmp)) {
          names(da_tmp)[names(da_tmp) == "."] <- "X."
        } else {
          showNotification(
            HTML(paste(
              "The column name", code("."), "(dot) is not allowed. Automatically renaming this",
              "column to", code("X."), "failed since there already exists a column",
              code("X.", .noWS = "after"), "."
            )),
            duration = NA,
            type = "error"
          )
          req(FALSE)
        }
      }
    }
    return(da_tmp)
  })
  
  ### Data preview ----------------------------------------------------------
  
  output$da_view <- renderTable({
    if (identical(input$preview_rows_radio, "head")) {
      return(head(da()))
    } else {
      return(da())
    }
  })
  
  output$da_str <- renderPrint({
    str(da())
  })
  
  ## Likelihood -------------------------------------------------------------
  
  ### Outcome ---------------------------------------------------------------
  
  observe({
    outc_choices <- c("Choose outcome ..." = "")
    if (!inherits(try(da(), silent = TRUE), "try-error")) {
      outc_choices <- c(outc_choices,
                        setdiff(names(da()),
                                c(input$pred_mainCP_sel,
                                  input$pred_mainPP_sel,
                                  input$offs_sel)))
      outc_slctd <- isolate(input$outc_sel)
    } else {
      outc_slctd <- NULL
    }
    updateSelectInput(session, "outc_sel",
                      choices = outc_choices,
                      selected = outc_slctd)
  })
  
  #### Distributional family ------------------------------------------------
  
  C_family <- reactive({
    req(input$dist_sel)
    return(brms::brmsfamily(family = input$dist_sel))
  })
  
  output$dist_link <- renderTable({
    if (identical(input$dist_sel, "")) {
      return(
        data.frame("Parameter" = character(),
                   "Link function" = character(),
                   check.names = FALSE)
      )
    } else {
      C_family_list <- C_family()
      dist_link_tmp <- data.frame("Parameter" = C_family_list$dpars,
                                  "Link function" = NA,
                                  check.names = FALSE)
      dist_link_tmp$"Link function" <- sapply(dist_link_tmp$"Parameter", function(par_i) {
        if (paste0("link_", par_i) %in% names(C_family_list)) {
          return(C_family_list[[paste0("link_", par_i)]])
        } else {
          return(NA)
        }
      })
      dist_link_tmp$"Link function"[dist_link_tmp$"Parameter" %in% c("mu")] <- C_family_list$link
      return(dist_link_tmp)
    }
  })
  
  ### Predictors ------------------------------------------------------------
  
  #### Main effects ---------------------------------------------------------
  
  observe({
    pred_mainCP_choices <- c("Choose variables for pooled main effects ..." = "")
    if (!inherits(try(da(), silent = TRUE), "try-error")) {
      pred_mainCP_choices <- c(pred_mainCP_choices,
                               setdiff(names(da()),
                                       c(input$outc_sel,
                                         input$pred_mainPP_sel,
                                         input$offs_sel)))
      pred_mainCP_slctd <- isolate(input$pred_mainCP_sel)
    } else {
      pred_mainCP_slctd <- NULL
    }
    updateSelectInput(session, "pred_mainCP_sel",
                      choices = pred_mainCP_choices,
                      selected = pred_mainCP_slctd)
  })
  
  observe({
    pred_mainPP_choices <- c("Choose variables for partially pooled main effects ..." = "")
    if (!inherits(try(da(), silent = TRUE), "try-error")) {
      PP_sel_choices <- setdiff(names(da()),
                                c(input$outc_sel,
                                  input$pred_mainCP_sel,
                                  input$offs_sel))
      if (length(PP_sel_choices) > 0L) {
        # Only allow factor, character, and logical variables:
        PP_sel_choices_OK <- sapply(da()[PP_sel_choices], function(x) {
          is.character(x) || is.factor(x) || is.logical(x)
        })
        PP_sel_choices <- PP_sel_choices[PP_sel_choices_OK]
      }
      pred_mainPP_choices <- c(pred_mainPP_choices, PP_sel_choices)
      pred_mainPP_slctd <- isolate(input$pred_mainPP_sel)
    } else {
      pred_mainPP_slctd <- NULL
    }
    updateSelectInput(session, "pred_mainPP_sel",
                      choices = pred_mainPP_choices,
                      selected = pred_mainPP_slctd)
  })
  
  #### Interactions ---------------------------------------------------------
  
  observe({
    pred_intBuild_choices <- c("Choose variables for an interaction term ..." = "")
    if (!inherits(try(da(), silent = TRUE), "try-error")) {
      pred_intBuild_choices <- c(pred_intBuild_choices,
                                 input$pred_mainCP_sel,
                                 input$pred_mainPP_sel)
      pred_int_slctd <- isolate(input$pred_int_build)
    } else {
      pred_int_slctd <- NULL
    }
    updateSelectInput(session, "pred_int_build",
                      choices = pred_intBuild_choices,
                      selected = pred_int_slctd)
  })
  
  pred_int_rv <- reactiveValues()
  observeEvent(input$pred_int_add, {
    if (length(input$pred_int_build) > 1L) {
      pred_int_rv$choices <- c(pred_int_rv$choices,
                               list(input$pred_int_build))
      pred_int_rv$choices <- pred_int_rv$choices[!duplicated(lapply(pred_int_rv$choices, sort))]
      pred_int_rv$choices_chr <- sapply(pred_int_rv$choices, paste, collapse = "<-->")
      updateSelectInput(session, "pred_int_sel",
                        choices = pred_int_rv$choices_chr,
                        selected = c(input$pred_int_sel,
                                     paste(input$pred_int_build, collapse = "<-->")))
      updateSelectInput(session, "pred_int_build",
                        choices = c("Choose variables for an interaction term ..." = "",
                                    input$pred_mainCP_sel,
                                    input$pred_mainPP_sel))
    }
  })
  
  # Ensure that all variables involved in the interaction terms have a main effect (either
  # pooled or partially pooled):
  observeEvent({
    input$pred_mainCP_sel
    input$pred_mainPP_sel
  }, {
    pred_intSel_slctd <- pred_int_rv$choices[pred_int_rv$choices_chr %in% input$pred_int_sel]
    pred_int_rv$choices <- lapply(pred_int_rv$choices, function(x) {
      intersect(x, c(input$pred_mainCP_sel,
                     input$pred_mainPP_sel))
    })
    pred_int_rv$choices <- pred_int_rv$choices[sapply(pred_int_rv$choices, length) > 1L]
    if (length(pred_int_rv$choices) > 0L) {
      pred_int_rv$choices_chr <- sapply(pred_int_rv$choices, paste, collapse = "<-->")
      pred_intSel_choices <- pred_int_rv$choices_chr
      pred_intSel_slctd <- lapply(pred_intSel_slctd, function(x) {
        intersect(x, c(input$pred_mainCP_sel,
                       input$pred_mainPP_sel))
      })
      pred_intSel_slctd <- pred_intSel_slctd[sapply(pred_intSel_slctd, length) > 1L]
      if (length(pred_intSel_slctd) > 0L) {
        pred_intSel_slctd <- sapply(pred_intSel_slctd, paste, collapse = "<-->")
      } else {
        pred_intSel_slctd <- NULL
      }
    } else {
      pred_int_rv$choices <- NULL
      pred_int_rv$choices_chr <- NULL
      pred_intSel_choices <- character()
      pred_intSel_slctd <- NULL
    }
    updateSelectInput(session, "pred_int_sel",
                      choices = pred_intSel_choices,
                      selected = pred_intSel_slctd)
  }, ignoreNULL = FALSE)
  
  #### Offsets --------------------------------------------------------------
  
  observe({
    offs_choices <- c("Choose variables for offsets ..." = "")
    if (!inherits(try(da(), silent = TRUE), "try-error")) {
      offs_choices <- c(offs_choices,
                        setdiff(names(da()),
                                c(input$outc_sel,
                                  input$pred_mainCP_sel,
                                  input$pred_mainPP_sel)))
      offs_slctd <- isolate(input$offs_sel)
    } else {
      offs_slctd <- NULL
    }
    updateSelectInput(session, "offs_sel",
                      choices = offs_choices,
                      selected = offs_slctd)
  })
  
  #### Combination of all chosen predictor terms ----------------------------
  
  C_pred <- reactive({
    if (is.null(input$pred_mainCP_sel) && is.null(input$pred_mainPP_sel)) {
      mainCP_tmp <- "1"
      if (length(input$offs_sel) > 0L) {
        mainCP_tmp <- c(mainCP_tmp, paste0("offset(", input$offs_sel, ")"))
      }
      return(data.frame("from_mainPP" = factor(NA_character_, levels = NA_character_, exclude = NULL),
                        "from_mainCP" = paste(mainCP_tmp, collapse = " + ")))
    }
    
    pred_lst <- c(
      as.list(input$pred_mainCP_sel),
      as.list(input$pred_mainPP_sel),
      pred_int_rv$choices[pred_int_rv$choices_chr %in% input$pred_int_sel]
    )
    if (length(input$pred_int_sel) > 0L) {
      # Perform the following tasks (at the same time):
      #   - Expand interactions on the group-level side (in principle, this is not necessary as the
      #     "*" syntax (<predictor_1>*<predictor_2>) also works on the group-level side; however, for
      #     including correlations between the partially pooled effects of a specific group-level term, the
      #     terms on the population-level side need to be grouped by the term on the group-level side).
      #   - For partially pooled slopes, add the corresponding pooled slopes since the partially pooled
      #     slopes are assumed to have mean zero.
      # The first task is performed by applying combn() to m = 1L, ..., length(xPP) with "xPP"
      # containing the group-level terms of a given element of "pred_lst".
      # The second task is performed by additionally applying combn() to m = 0L when performing
      # the first task.
      pred_needsExpand <- sapply(pred_lst, function(x) {
        sum(x %in% input$pred_mainPP_sel) > 0L
      })
      if (any(pred_needsExpand)) { # This if () condition is not necessary, but included for better readability.
        pred_lst_toExpand <- pred_lst[pred_needsExpand]
        pred_lst_expanded <- do.call("c", lapply(pred_lst_toExpand, function(x) {
          xPP <- intersect(x, input$pred_mainPP_sel)
          xPP_lst_expanded <- unlist(lapply(c(0L, seq_along(xPP)), combn, x = xPP, simplify = FALSE),
                                     recursive = FALSE)
          xCP <- intersect(x, input$pred_mainCP_sel)
          lapply(xPP_lst_expanded, "c", xCP)
        }))
        pred_lst <- c(pred_lst[!pred_needsExpand],
                      pred_lst_expanded)
      }
      
      # Remove duplicates:
      pred_lst <- pred_lst[!duplicated(lapply(pred_lst, sort))]
      
      # By group-level term: Check each population-level term for being a "subterm" (lower-order
      # term) of a high-order term and if yes, remove it:
      pred_vec_chr <- sapply(pred_lst, function(x) {
        xPP <- intersect(x, input$pred_mainPP_sel)
        if (length(xPP) > 0L) {
          return(paste(xPP, collapse = "<-->"))
        } else {
          return(NA_character_)
        }
      })
      pred_vec_chr <- factor(pred_vec_chr, levels = unique(pred_vec_chr), exclude = NULL)
      pred_lst <- tapply(pred_lst, pred_vec_chr, function(x_lst) {
        xCP_lst <- lapply(x_lst, intersect, y = input$pred_mainCP_sel)
        x_isSubCP <- sapply(seq_along(xCP_lst), function(idx) {
          any(sapply(xCP_lst[-idx], function(xCP) {
            all(xCP_lst[[idx]] %in% xCP)
          }))
        })
        return(x_lst[!x_isSubCP])
      }, simplify = FALSE)
      pred_lst <- unlist(pred_lst, recursive = FALSE, use.names = FALSE)
    }
    pred_lst <- c(pred_lst, as.list(input$offs_sel))
    
    pred_DF <- do.call("rbind", lapply(pred_lst, function(x) {
      xCP <- intersect(x, input$pred_mainCP_sel)
      if (length(xCP) > 0L) {
        xCP <- paste(xCP, collapse = "*")
      } else {
        xCP <- NA_character_
      }
      xPP <- intersect(x, input$pred_mainPP_sel)
      if (length(xPP) > 0L) {
        xPP <- paste(xPP, collapse = ":")
      } else {
        xPP <- NA_character_
      }
      xOffs <- intersect(x, input$offs_sel)
      if (identical(length(xOffs), 1L)) {
        if (!isTRUE(is.na(xCP))) {
          stop("Unexpected value of `xCP`. Please report this.")
        }
        xCP <- paste0("offset(", xOffs, ")")
      } else if (!identical(length(xOffs), 0L)) {
        stop("Unexpected length of `xOffs`. Please report this.")
      }
      data.frame("from_mainCP" = xCP,
                 "from_mainPP" = xPP)
    }))
    pred_DF$from_mainPP <- factor(pred_DF$from_mainPP, levels = unique(pred_DF$from_mainPP), exclude = NULL)
    pred_DF <- aggregate(from_mainCP ~ from_mainPP, pred_DF, function(x) {
      paste(c("1", x[!is.na(x)]), collapse = " + ")
    }, na.action = na.pass)
    return(pred_DF)
  })
  
  #### Predictor terms preview ----------------------------------------------
  
  output$pred_view <- renderTable({
    C_pred()
  }, sanitize.colnames.function = function(x) {
    x <- sub("^from_mainCP$", "Effect(s)", x)
    x <- sub("^from_mainPP$", "Group", x)
    return(x)
  })
  
  ### Formula ---------------------------------------------------------------
  
  C_formula_char <- reactive({
    req(input$outc_sel)
    
    formula_splitted <- apply(C_pred(), 1, function(x) {
      if (is.na(x["from_mainPP"])) {
        return(x["from_mainCP"])
      } else {
        return(paste0("(", x["from_mainCP"], " | ", x["from_mainPP"], ")"))
      }
    })
    return(paste(
      input$outc_sel,
      "~",
      paste(formula_splitted, collapse = " + ")
    ))
  })
  
  C_formula <- reactive({
    req(C_formula_char())
    return(as.formula(C_formula_char()))
  })
  
  output$formula_view <- renderText({
    C_formula_char()
  })
  
  ## Prior ------------------------------------------------------------------
  
  ### Prior construction ----------------------------------------------------
  
  C_prior_rv <- reactiveValues(prior_set_obj = brms::empty_prior())
  
  # Get default priors:
  C_prior_default <- reactive({
    if(inherits(try(C_formula(), silent = TRUE), "try-error") ||
       inherits(try(C_family(), silent = TRUE), "try-error") ||
       inherits(try(req(all(c(
         setdiff(input$outc_sel, ""),
         input$pred_mainCP_sel,
         input$pred_mainPP_sel,
         input$offs_sel
       ) %in% names(da()))), silent = TRUE), "try-error")){
      return(brms::empty_prior())
    }
    warn_orig <- options(warn = 1)
    warn_capt <- capture.output({
      C_prior_default_tmp <- try(brms::get_prior(formula = C_formula(),
                                                 data = da(),
                                                 family = C_family()),
                                 silent = TRUE)
    }, type = "message")
    options(warn = warn_orig$warn)
    if (inherits(C_prior_default_tmp, "try-error")) {
      ### Option 1:
      # err_capt <- attr(C_prior_default_tmp, "condition")$message
      ###
      ### Option 2:
      err_capt <- conditionMessage(attr(C_prior_default_tmp, "condition"))
      ###
      for (err_capt_i in err_capt) {
        if (!identical(err_capt_i, "")) {
          showNotification(err_capt_i, duration = NA, type = "error")
        }
      }
      # To avoid running Stan in this case, throw a silent error instead of having
      # `return(brms::empty_prior())`:
      req(FALSE)
    }
    if (length(warn_capt) > 0L) {
      warn_capt <- unique(warn_capt)
      warn_capt[warn_capt == "Warning: Rows containing NAs were excluded from the model."] <- paste(
        "Warning: There are missing values in the data. The corresponding rows have been",
        "omitted in the construction of the default priors. They will also be omitted when",
        "running Stan (and also in the Stan data)."
      )
      for (warn_capt_i in warn_capt) {
        showNotification(
          warn_capt_i,
          duration = NA,
          type = "warning"
        )
      }
    }
    return(C_prior_default_tmp)
  })
  
  # Update the choices for "class" (if necessary):
  observe({
    prior_class_choices <- unique(c("", C_prior_default()$class))
    prior_class_choices <- setNames(prior_class_choices, prior_class_choices)
    names(prior_class_choices)[prior_class_choices == ""] <- "Choose class ..."
    
    prior_class_slctd <- intersect(prior_class_choices,
                                   isolate(input$prior_class_sel))
    prior_class_slctd <- setNames(prior_class_slctd, prior_class_slctd)
    names(prior_class_slctd)[prior_class_slctd == ""] <- "Choose class ..."
    if (identical(length(prior_class_slctd), 0L)) {
      prior_class_slctd <- NULL
    }
    
    updateSelectInput(session, "prior_class_sel",
                      choices = prior_class_choices,
                      selected = prior_class_slctd)
  })
  
  # Update the choices for "coefficient" (if necessary):
  observe({
    prior_coef_choices <- unique(C_prior_default()$coef[
      C_prior_default()$class %in% input$prior_class_sel
    ])
    if (identical(length(prior_coef_choices), 0L)) {
      prior_coef_choices <- ""
    }
    prior_coef_choices <- setNames(prior_coef_choices, prior_coef_choices)
    names(prior_coef_choices)[prior_coef_choices == ""] <- "Choose coefficient or leave empty"
    
    prior_coef_slctd <- intersect(prior_coef_choices,
                                  isolate(input$prior_coef_sel))
    prior_coef_slctd <- setNames(prior_coef_slctd, prior_coef_slctd)
    names(prior_coef_slctd)[prior_coef_slctd == ""] <- "Choose coefficient or leave empty"
    if (identical(length(prior_coef_slctd), 0L)) {
      prior_coef_slctd <- NULL
    }
    
    updateSelectInput(session, "prior_coef_sel",
                      choices = prior_coef_choices,
                      selected = prior_coef_slctd)
  })
  
  # Update the choices for "group" (if necessary):
  observe({
    prior_group_choices <- unique(C_prior_default()$group[
      C_prior_default()$class %in% input$prior_class_sel &
        C_prior_default()$coef %in% input$prior_coef_sel
    ])
    if (identical(length(prior_group_choices), 0L)) {
      prior_group_choices <- ""
    }
    prior_group_choices <- setNames(prior_group_choices, prior_group_choices)
    names(prior_group_choices)[prior_group_choices == ""] <- "Choose group or leave empty"
    
    prior_group_slctd <- intersect(prior_group_choices,
                                   isolate(input$prior_group_sel))
    prior_group_slctd <- setNames(prior_group_slctd, prior_group_slctd)
    names(prior_group_slctd)[prior_group_slctd == ""] <- "Choose group or leave empty"
    if (identical(length(prior_group_slctd), 0L)) {
      prior_group_slctd <- NULL
    }
    
    updateSelectInput(session, "prior_group_sel",
                      choices = prior_group_choices,
                      selected = prior_group_slctd)
  })
  
  # Reset the custom priors if the default prior changes:
  observeEvent(C_prior_default(), {
    C_prior_rv$prior_set_obj <- brms::empty_prior()
  })
  
  # Add a custom prior if the user clicks the corresponding button:
  observeEvent(input$prior_add, {
    req(input$prior_class_sel)
    prior_text_valid <- identical(input$prior_text, "") ||
      any(sapply(prior_stan_fun, function(prior_stan_fun_i) {
        grepl(paste0("^", prior_stan_fun_i, "\\([[:digit:][:blank:].,]*\\)$"), input$prior_text)
      })) ||
      grepl(paste0("^horseshoe\\((",
                   "(",
                   "(df[[:blank:]]*=[[:blank:]]*)|",
                   "(scale_global[[:blank:]]*=[[:blank:]]*)|",
                   "(df_global[[:blank:]]*=[[:blank:]]*)|",
                   "(scale_slab[[:blank:]]*=[[:blank:]]*)|",
                   "(df_slab[[:blank:]]*=[[:blank:]]*)|",
                   "(par_ratio[[:blank:]]*=[[:blank:]]*)|",
                   "(autoscale[[:blank:]]*=[[:blank:]]*)|",
                   "",
                   ")",
                   "([[:digit:][:blank:].,]*|NULL|TRUE|FALSE)",
                   ")*\\)$"), input$prior_text) ||
      grepl(paste0("^lasso\\((",
                   "(",
                   "(df[[:blank:]]*=[[:blank:]]*)|",
                   "(scale[[:blank:]]*=[[:blank:]]*)|",
                   "",
                   ")",
                   "[[:digit:][:blank:].,]*",
                   ")*\\)$"), input$prior_text) ||
      grepl(paste0("^dirichlet\\([[:digit:][:blank:].,:c()]*\\)$"), input$prior_text) ||
      any(sapply(setdiff(prior_brms_fun, c("horseshoe", "lasso", "dirichlet")), function(prior_brms_fun_i) {
        grepl(paste0("^", prior_brms_fun_i, "\\([[:digit:][:blank:].]*\\)$"), input$prior_text)
      }))
    if (!prior_text_valid) {
      showNotification(
        paste("Your custom prior has not been added since your text in the \"Prior distribution\"",
              "input field could not be recognized."),
        duration = NA,
        type = "error"
      )
      return()
    }
    prior_set_obj_add <- brms::set_prior(prior = input$prior_text,
                                         class = input$prior_class_sel,
                                         coef = input$prior_coef_sel,
                                         group = input$prior_group_sel)
    prior_set_obj_add_ch <- merge(prior_set_obj_add[, !names(prior_set_obj_add) %in% c("prior", "source")],
                                  C_prior_default()[, !names(C_prior_default()) %in% c("prior", "source")],
                                  sort = FALSE)
    class(prior_set_obj_add_ch) <- c("brmsprior", "data.frame")
    if (!identical(prior_set_obj_add_ch,
                   prior_set_obj_add[, !names(prior_set_obj_add) %in% c("prior", "source")])) {
      showNotification(
        paste("Your custom prior has not been added since the combination of",
              "\"Class\", \"Coefficient\", and \"Group\" you have currently selected is",
              "not contained in the table of the default priors."),
        duration = NA,
        type = "error"
      )
      return()
    }
    C_prior_rv$prior_set_obj <- prior_set_obj_add + C_prior_rv$prior_set_obj
    C_prior_rv$prior_set_obj <- unique(C_prior_rv$prior_set_obj)
  })
  
  # Reset the custom priors if the user clicks the corresponding button:
  observeEvent(input$prior_reset, {
    C_prior_rv$prior_set_obj <- brms::empty_prior()
  })
  
  # A `reactive()` object containing the custom prior (only necessary to be able
  # to raise a silent error similar to `req(FALSE)` which is not possible for
  # `reactiveValues`):
  C_prior <- reactive({
    req(C_prior_default())
    return(C_prior_rv$prior_set_obj)
  })
  
  ### Prior preview ---------------------------------------------------------
  
  prior_colsToHide <- reactive({
    return(
      names(C_prior_default()) == "source" |
        (sapply(C_prior_default(), function(x) {
          is.character(x) && all(x == "")
        }) &
          !grepl("^prior$|^class$|^coef$|^group$", names(C_prior_default())))
    )
  })
  
  output$prior_default_view <- renderTable({
    C_prior_default()[, !prior_colsToHide()]
  }, sanitize.colnames.function = san_prior_tab_nms)
  
  output$prior_set_view <- renderTable({
    C_prior()[, !prior_colsToHide()]
  }, sanitize.colnames.function = san_prior_tab_nms)
  
  ## Posterior --------------------------------------------------------------
  
  ### Run Stan --------------------------------------------------------------
  
  #### Stan code ------------------------------------------------------------
  
  C_stancode <- reactive({
    req(C_formula(), C_family(), C_prior())
    warn_orig <- options(warn = 1)
    warn_capt <- capture.output({
      C_stancode_tmp <- brms::make_stancode(formula = C_formula(),
                                            data = da(),
                                            family = C_family(),
                                            prior = C_prior())
    }, type = "message")
    options(warn = warn_orig$warn)
    if (length(warn_capt) > 0L) {
      warn_capt <- unique(warn_capt)
      warn_capt[warn_capt == "Warning: Rows containing NAs were excluded from the model."] <-
        paste("Warning: There are missing values in the data. The corresponding rows will be",
              "omitted when running Stan (and also in the Stan data).")
      for (warn_capt_i in warn_capt) {
        showNotification(
          warn_capt_i,
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
    content = function(file) {
      cat(C_stancode(), file = file)
    }
  )
  
  #### Stan data ------------------------------------------------------------
  
  C_standata <- reactive({
    req(C_formula(), C_family(), C_prior())
    warn_orig <- options(warn = 1)
    warn_capt <- capture.output({
      C_standata_tmp <- brms::make_standata(formula = C_formula(),
                                            data = da(),
                                            family = C_family(),
                                            prior = C_prior())
    }, type = "message")
    options(warn = warn_orig$warn)
    if (length(warn_capt) > 0L) {
      warn_capt <- unique(warn_capt)
      warn_capt[warn_capt == "Warning: Rows containing NAs were excluded from the model."] <-
        paste("Warning: There are missing values in the data. The corresponding rows have been",
              "omitted in the Stan data. They will also be omitted when running Stan.")
      for (warn_capt_i in warn_capt) {
        showNotification(
          warn_capt_i,
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
    content = function(file) {
      saveRDS(C_standata(), file = file)
    }
  )
  
  #### Run Stan -------------------------------------------------------------
  
  reset_brmsfit_upload <- reactiveVal()
  C_bfit_raw <- reactiveVal()
  
  observeEvent(input$run_stan, {
    req(C_formula(), C_family(), C_prior(),
        input$advOpts_cores,
        input$advOpts_chains,
        input$advOpts_iter,
        input$advOpts_thin,
        input$advOpts_adapt_delta,
        input$advOpts_max_treedepth)
    
    save_warmup_tmp <- input$advOpts_save_warmup
    if (identical(input$advOpts_backend, "cmdstanr")) {
      if (!requireNamespace("cmdstanr", quietly = TRUE)) {
        showNotification(
          HTML(paste("Package", strong("cmdstanr"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
      if (save_warmup_tmp) {
        showNotification(
          HTML(paste0(
            "Because of ", strong("brms", .noWS = "outside"), "'s issue #1257 ",
            "(see GitHub), saving the warmup draws is currently not possible ",
            "if the ", strong("cmdstanr", .noWS = "outside"), " backend is ",
            "used. Now deselecting this option internally."
          )),
          duration = NA,
          type = "warning"
        )
        save_warmup_tmp <- FALSE
      }
    }
    
    args_brm <- list(
      formula = C_formula(),
      data = da(),
      family = C_family(),
      prior = C_prior(),
      backend = input$advOpts_backend,
      cores = min(input$advOpts_cores, input$advOpts_chains),
      chains = input$advOpts_chains,
      seed = input$advOpts_seed,
      iter = input$advOpts_iter,
      thin = input$advOpts_thin,
      inits = input$advOpts_inits,
      save_pars = brms::save_pars(all = input$advOpts_save_all_pars),
      save_warmup = save_warmup_tmp
    )
    if (!is.na(input$advOpts_warmup)) {
      args_brm <- c(args_brm,
                    list(warmup = input$advOpts_warmup))
    }
    if (!is.na(input$advOpts_refresh)) {
      args_brm <- c(args_brm,
                    list(refresh = input$advOpts_refresh))
    }
    if (identical(input$advOpts_backend, "cmdstanr")) {
      args_brm <- c(
        args_brm,
        list(adapt_delta = input$advOpts_adapt_delta,
             max_treedepth = input$advOpts_max_treedepth)
      )
      if (!is.na(input$advOpts_init_r)) {
        args_brm <- c(args_brm,
                      list(init = input$advOpts_init_r))
      }
    } else {
      args_brm <- c(
        args_brm,
        list(open_progress = input$advOpts_open_progress,
             control = list(adapt_delta = input$advOpts_adapt_delta,
                            max_treedepth = input$advOpts_max_treedepth))
      )
      if (!is.na(input$advOpts_init_r)) {
        args_brm <- c(args_brm,
                      list(init_r = input$advOpts_init_r))
      }
    }
    
    # Logical (single value) indicating whether to use brms:::update.brmsfit():
    use_upd <- isTRUE(getOption("shinybrms.allow_upd", TRUE)) &&
      # In fact, rlang::hash() should never return `NULL`, so the following line
      # is not strictly necessary, but it doesn't harm either:
      !is.null(C_bfit_raw()) &&
      # In fact, rlang::hash() should never return the value of
      # `da_hash_no_data`, so the following line is not strictly necessary, but
      # it doesn't harm either:
      !C_bfit_raw()$is_upload &&
      # Only use brms:::update.brmsfit() if the dataset has not changed (because
      # brms:::update.brmsfit() does not recompute the default priors if the
      # dataset has changed):
      identical(rlang::hash(da()), C_bfit_raw()$da_hash)
    if (use_upd &&
        identical(C_bfit_raw()$bfit$backend, "rstan") &&
        identical(args_brm$backend, "cmdstanr")) {
      # Handle **brms** issue #1259 explicitly:
      use_upd <- FALSE
    }
    
    if (use_upd) {
      run_mssg <- paste(
        "Stan will now compile the C++ code for your model (if necessary; this",
        "may take a while) and will then start sampling."
      )
    } else {
      run_mssg <- paste(
        "Stan will now compile the C++ code for your model (which may take a",
        "while) and will then start sampling."
      )
    }
    showNotification(run_mssg, duration = 60, type = "message")
    
    # Some modifications needed to show the progress (see the source code of rstan::sampling()):
    if (input$advOpts_open_progress) {
      # In RStudio, we have `identical(Sys.getenv("RSTUDIO"), "1")`, but we need
      # `!identical(Sys.getenv("RSTUDIO"), "1")`, so use `""` which should be
      # used outside of RStudio:
      RSTUDIO_orig <- Sys.getenv("RSTUDIO")
      Sys.setenv("RSTUDIO" = "")
      
      # The progress browser:
      prog_browser <- getOption("shinybrms.prog_browser",
                                getOption("browser"))
      if (is.function(prog_browser) &&
          any(grepl("rs_browseURL|rs_shinyviewer", as.character(body(prog_browser))))) {
        # In this case, "prog_browser" cannot be used (at least not without requiring the user to
        # perform some major modifications to the initialization of the R session), so use the
        # default browser stored in the environment variable "R_BROWSER":
        prog_browser <- Sys.getenv("R_BROWSER")
        if (identical(.Platform$OS.type, "windows") &&
            identical(prog_browser, "")) {
          prog_browser <- NULL
        }
      }
      browser_orig <- options(browser = prog_browser)
      
      # Even show the progress if parallel::mclapply() with forking is intended
      # to be used or actually used (see the source code from `library(rstan); getMethod("sampling",
      # signature = "stanmodel")` for the condition when parallel::mclapply()
      # with forking is used) or if using the **cmdstanr** backend:
      if (identical(.Platform$OS.type, "unix") &&
          interactive() &&
          isatty(stdout())) {
        # In this case, the simplest solution to make the progress file open up
        # is to avoid forking:
        sink(tempfile(pattern = "shinybrms_dummy_stdout_", fileext = ".txt"))
        sink_active <- TRUE
      } else if ((identical(.Platform$OS.type, "unix") &&
                  !interactive()) ||
                 identical(input$advOpts_backend, "cmdstanr")) {
        # In this case, create an own progress file to be opened up (and don't
        # avoid forking):
        tmp_stdout_txt <- tempfile(pattern = "shinybrms_stdout_", fileext = ".txt")
        sink(tmp_stdout_txt)
        sink_active <- TRUE
        cat("Refresh this page to see the sampling progress.",
            "Note that the C++ code for your model might need to be compiled",
            "first, which may take a while.\n")
        tmp_stdout_html <- sub("\\.txt$", ".html", tmp_stdout_txt)
        rstan:::create_progress_html_file(tmp_stdout_html, tmp_stdout_txt)
        browseURL(paste0("file://", tmp_stdout_html))
      }
    }
    
    # Get warnings directly when they occur:
    warn_orig <- options(warn = 1)
    
    # Run Stan (more precisely: brms::brm() (or brms:::update.brmsfit(), if possible)):
    if (use_upd) {
      # Note: The try() call was added for the case where a `brmsfit` is updated
      # by *extending* the predictors. However, it also handles **brms** issue #1259
      # implicitly.
      warn_capt <- capture.output({
        bfit_tmp <- try(do.call(update, args = c(
          list(object = C_bfit_raw()$bfit,
               formula. = C_formula()),
          args_brm[setdiff(names(args_brm), c("formula", "data"))]
        )), silent = TRUE)
      }, type = "message")
    }
    if (!use_upd ||
        ### Should in fact be redundant, given the `!use_upd` condition (but
        ### shouldn't harm either):
        !exists("bfit_tmp") ||
        ### 
        (exists("bfit_tmp") && inherits(bfit_tmp, "try-error"))) {
      warn_capt <- capture.output({
        bfit_tmp <- do.call(brms::brm, args = args_brm)
      }, type = "message")
    }
    
    # Reset all modified options and environment variables:
    options(warn = warn_orig$warn)
    if (exists("sink_active")) sink()
    if (exists("browser_orig")) options(browser = browser_orig$browser)
    if (exists("RSTUDIO_orig")) Sys.setenv("RSTUDIO" = RSTUDIO_orig)
    
    # Notifications for the warnings thrown by the call to brms::brm():
    if (length(warn_capt) > 0L) {
      warn_capt <- unique(warn_capt)
      warn_capt[warn_capt == "Warning: Rows containing NAs were excluded from the model."] <-
        paste("Warning: There were missing values in the dataset which was used for the model.",
              "The corresponding rows have been omitted in the Stan run.")
      warn_capt <- setdiff(warn_capt, c(
        "Compiling Stan program...",
        "Start sampling",
        "recompiling to avoid crashing R session",
        grep("Warning: There were [[:digit:]]+ divergent transitions after warmup\\. See", warn_capt, value = TRUE),
        grep("^[[:space:]]*$", warn_capt, value = TRUE),
        "http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup",
        "to find out why this is a problem and how to eliminate them.",
        "Warning: Examine the pairs() plot to diagnose sampling problems"
      ))
      for (warn_capt_i in warn_capt) {
        showNotification(
          warn_capt_i,
          duration = NA,
          type = "warning"
        )
      }
    }
    
    C_bfit_raw(list(bfit = bfit_tmp,
                    is_upload = FALSE,
                    n_chains_spec = input$advOpts_chains,
                    da_hash = rlang::hash(da())))
    reset_brmsfit_upload("dummy_value")
  })
  
  output$brmsfit_upload_UI <- renderUI({
    reset_brmsfit_upload()
    fileInput("brmsfit_upload", "Upload \"brmsfit\" object (RDS file):",
              multiple = FALSE,
              accept = c(".rds"),
              width = "320px",
              buttonLabel = "Browse ...")
  })
  
  observeEvent(input$brmsfit_upload, {
    req(input$brmsfit_upload)
    bfit_tmp <- try(readRDS(input$brmsfit_upload$datapath),
                    silent = TRUE)
    if (inherits(bfit_tmp, "try-error")) {
      showModal(modalDialog(
        "The file upload failed.",
        title = "File upload failed",
        footer = modalButton("Close"),
        size = "s",
        easyClose = TRUE
      ))
      req(FALSE)
    }
    C_bfit_raw(list(bfit = bfit_tmp,
                    is_upload = TRUE,
                    n_chains_spec = -Inf,
                    da_hash = da_hash_no_data))
  })
  
  C_stanres <- reactive({
    invisible(req(C_bfit_raw()))
    C_draws_arr <- as.array(C_bfit_raw()$bfit)
    n_chains_out <- dim(C_draws_arr)[2]
    C_sfit <- C_bfit_raw()$bfit$fit
    stopifnot(rstan:::is.stanfit(C_sfit))
    
    # Check that the mode of the resulting "stanfit" object is the "normal" mode
    # (0L), i.e. neither test gradient mode (1L) nor error mode (2L):
    stopifnot(identical(C_sfit@mode, 0L))
    
    ##### Computation of MCMC diagnostics -------------------------------------
    
    ###### HMC-specific diagnostics -------------------------------------------
    
    C_div <- rstan::get_num_divergent(C_sfit)
    C_div_OK <- identical(C_div, 0L)
    
    C_tree <- rstan::get_num_max_treedepth(C_sfit)
    C_tree_OK <- identical(C_tree, 0L)
    
    C_EBFMI <- setNames(rstan::get_bfmi(C_sfit),
                        paste0("chain_", sapply(C_sfit@stan_args, "[[", "chain_id")))
    C_EBFMI_OK <- all(C_EBFMI >= 0.2)
    
    ###### General MCMC diagnostics -------------------------------------------
    
    C_essBulk <- apply(C_draws_arr, MARGIN = 3, FUN = rstan::ess_bulk)
    if (any(is.na(C_essBulk))) {
      C_essBulk_OK <- FALSE
    } else {
      C_essBulk_OK <- all(C_essBulk > 100 * n_chains_out)
    }
    
    C_rhat <- apply(C_draws_arr, MARGIN = 3, FUN = rstan::Rhat)
    if (any(is.na(C_rhat))) {
      C_rhat_OK <- FALSE
    } else {
      C_rhat_OK <- all(C_rhat < 1.01)
    }
    
    C_essTail <- apply(C_draws_arr, MARGIN = 3, FUN = rstan::ess_tail)
    if (any(is.na(C_essTail))) {
      C_essTail_OK <- FALSE
    } else {
      C_essTail_OK <- all(C_essTail > 100 * n_chains_out)
    }
    
    ###### Overall check for all MCMC diagnostics -----------------------------
    
    C_all_OK <- all(c(C_div_OK, C_tree_OK, C_EBFMI_OK,
                      C_essBulk_OK, C_rhat_OK, C_essTail_OK))
    
    ###### Notifications for the MCMC diagnostics -----------------------------
    
    # First: Check for failed chains:
    # Note: `n_chains_out < -Inf` is always `FALSE`, so the
    # `!C_bfit_raw()$is_upload` part is not strictly necessary, but it doesn't
    # harm either:
    if (!C_bfit_raw()$is_upload && n_chains_out < C_bfit_raw()$n_chains_spec) {
      showNotification(
        paste("Warning: Stan results obtained, but at least one chain exited with an error.",
              "The Stan results should not be used."),
        duration = NA,
        type = "warning"
      )
    } else {
      # Secondly: Overall check for all MCMC diagnostics:
      if (C_all_OK) {
        showNotification(
          paste("Stan results obtained. All MCMC diagnostics are OK (see",
                "the tab \"MCMC diagnostics\" for details)."),
          duration = NA,
          type = "message"
        )
      } else {
        showNotification(
          paste("Warning: Stan results obtained, but at least one MCMC diagnostic is worrying (see",
                "the tab \"MCMC diagnostics\" for details). In general,",
                "this indicates that the Stan results should not be used."),
          duration = NA,
          type = "warning"
        )
      }
    }
    
    return(list(bfit = C_bfit_raw()$bfit,
                diagn = list(all_OK = C_all_OK,
                             divergences_OK = C_div_OK,
                             divergences = C_div,
                             hits_max_tree_depth_OK = C_tree_OK,
                             hits_max_tree_depth = C_tree,
                             EBFMI_OK = C_EBFMI_OK,
                             EBFMI = C_EBFMI,
                             Rhat_OK = C_rhat_OK,
                             Rhat = C_rhat,
                             ESS_bulk_OK = C_essBulk_OK,
                             ESS_bulk = C_essBulk,
                             ESS_tail_OK = C_essTail_OK,
                             ESS_tail = C_essTail),
                draws_arr = C_draws_arr))
  })
  
  ##### Matrix of posterior draws (for later usage and only run if needed) ----
  
  C_draws_mat <- reactive({
    invisible(req(C_stanres()))
    return(as.matrix(C_stanres()$bfit))
  })
  
  ##### Date and time when the Stan run was finished ------------------------
  
  output$fit_date <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    C_stanres()$bfit$fit@date
  })
  
  ##### Versions used for this Stan run -------------------------------------
  
  output$fit_version <- renderPrint({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    unlist(lapply(C_stanres()$bfit$version, as.character))
  })
  
  ##### Overall check for all MCMC diagnostics ------------------------------
  
  output$diagn_all_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    if (C_stanres()$diagn$all_OK) {
      return(paste("All MCMC diagnostics are OK (see",
                   "the tab \"MCMC diagnostics\" for details)."))
    } else {
      return(paste("Warning: At least one MCMC diagnostic is worrying (see",
                   "the tab \"MCMC diagnostics\" for details). In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  ##### Download ------------------------------------------------------------
  
  output$stanout_download <- downloadHandler(
    filename = function() {
      return(input$stanout_download_sel)
    },
    content = function(file) {
      input$run_stan # Just for graying out.
      if (identical(input$stanout_download_sel, "shinybrms_post_draws_mat.csv")) {
        write.csv(C_draws_mat(),
                  file = file,
                  row.names = FALSE)
      } else {
        invisible(req(C_stanres()))
        saveRDS(switch(input$stanout_download_sel,
                       "shinybrms_brmsfit.rds" = C_stanres()$bfit,
                       "shinybrms_post_draws_mat.rds" = C_draws_mat(),
                       "shinybrms_post_draws_arr.rds" = C_stanres()$draws_arr),
                file = file)
      }
    }
  )
  
  ### MCMC diagnostics ------------------------------------------------------
  
  #### HMC-specific diagnostics ---------------------------------------------
  
  output$diagn_div_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    div_text <- paste0("The number of iterations ending with a divergence (",
                       C_stanres()$diagn$divergences,
                       ")")
    if (C_stanres()$diagn$divergences_OK) {
      return(paste(div_text, "is OK."))
    } else {
      return(paste("Warning:", div_text, "is worrying. In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  output$diagn_tree_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    tree_text <- paste0("The number of iterations hitting the maximum tree depth (",
                        C_stanres()$diagn$hits_max_tree_depth,
                        ")")
    if (C_stanres()$diagn$hits_max_tree_depth_OK) {
      return(paste(tree_text, "is OK."))
    } else {
      return(paste("Warning:", tree_text, "is worrying. In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  output$diagn_EBFMI_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    EBFMI_text <- paste0("The E-BFMI (",
                         paste(paste0(names(C_stanres()$diagn$EBFMI),
                                      ": ",
                                      format(round(C_stanres()$diagn$EBFMI, 4),
                                             nsmall = 4)),
                               collapse = ", "),
                         ")")
    if (C_stanres()$diagn$EBFMI_OK) {
      return(paste(EBFMI_text, "is OK."))
    } else {
      return(paste("Warning:", EBFMI_text, "is worrying. In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  #### General MCMC diagnostics ---------------------------------------------
  
  output$rhat_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    if (C_stanres()$diagn$Rhat_OK) {
      return("All R-hat values are OK.")
    } else {
      return(paste("Warning: At least one R-hat value is worrying. In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  output$essBulk_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    if (C_stanres()$diagn$ESS_bulk_OK) {
      return("All bulk-ESS values are OK.")
    } else {
      return(paste("Warning: At least one bulk-ESS value is worrying. In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  output$essTail_out <- renderText({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    if (C_stanres()$diagn$ESS_tail_OK) {
      return("All tail-ESS values are OK.")
    } else {
      return(paste("Warning: At least one tail-ESS value is worrying. In general,",
                   "this indicates that the Stan results should not be used."))
    }
  }, sep = "\n")
  
  output$general_MCMC_out <- renderPrint({
    input$run_stan # Just for graying out.
    invisible(req(C_stanres()))
    data.frame("R-hat" = C_stanres()$diagn$Rhat,
               "ESS_bulk" = C_stanres()$diagn$ESS_bulk,
               "ESS_tail" = C_stanres()$diagn$ESS_tail,
               check.names = FALSE)
  })
  
  #### Download -------------------------------------------------------------
  
  output$diagn_download <- downloadHandler(
    filename = "shinybrms_MCMC_diagnostics.rds",
    content = function(file) {
      input$run_stan # Just for graying out.
      invisible(req(C_stanres()))
      saveRDS(C_stanres()$diagn, file = file)
    }
  )
  
  ### Default summary -------------------------------------------------------
  
  C_smmry <- reactive({
    invisible(req(C_stanres()))
    summary(C_stanres()$bfit, robust = TRUE, priors = TRUE, prob = 0.95, mc_se = FALSE)
  })
  
  output$smmry_view <- renderPrint({
    input$run_stan # Just for graying out.
    print(C_smmry(), digits = 4)
  }, width = max(getOption("width"), 100))
  
  #### Download -------------------------------------------------------------
  
  output$smmry_download <- downloadHandler(
    filename = "shinybrms_default_summary.txt",
    content = function(file) {
      input$run_stan # Just for graying out.
      invisible(req(C_smmry()))
      sink(file = file)
      print(C_smmry(), digits = 4)
      sink()
    }
  )
  
  ### Custom summary --------------------------------------------------------
  
  observeEvent(input$cust_allow_link, {
    showModal(modalDialog(
      HTML(paste(
        "These are the characters and character groups which are allowed in",
        "the custom expression on tab \"Custom summary\":",
        tags$ul(
          lapply(cust_allow_all, function(char_i) {
            if (identical(char_i, " ")) {
              return(tags$li(HTML(paste(code(char_i), "&nbsp;(blank space)"))))
            }
            return(tags$li(code(char_i)))
          })
        )
      )),
      title = "Allowed characters and character groups",
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$cust_help_link, {
    showModal(modalDialog(
      HTML(paste(
        "Help pages for R functions which may be used in the custom expression:",
        tags$ul(
          tags$li(a("\"Arithmetic Operators\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Arithmetic.html",
                    target = "_blank")),
          tags$li(a("\"Relational Operators\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Comparison.html",
                    target = "_blank")),
          tags$li(a("\"Logical Operators\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Logic.html",
                    target = "_blank")),
          tags$li(a("\"Miscellaneous Mathematical Functions\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/MathFun.html",
                    target = "_blank"),
                  "(", code("abs()", .noWS = "before"), "and", code("sqrt()", .noWS = "after"), ")"),
          tags$li(a("\"Sign Function\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/sign.html",
                    target = "_blank")),
          tags$li(a("\"Rounding of Numbers\"",
                    href = "https://stat.ethz.ch/R-manual/R-patched/library/base/html/Round.html",
                    target = "_blank")),
          tags$li(a("\"Cumulative Sums, Products, and Extremes\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/cumsum.html",
                    target = "_blank")),
          tags$li(a("\"Logarithms and Exponentials\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Log.html",
                    target = "_blank")),
          tags$li(a("\"Trigonometric Functions\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Trig.html",
                    target = "_blank")),
          tags$li(a("\"Special Functions of Mathematics\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Special.html",
                    target = "_blank"),
                  "(", code("gamma()", .noWS = "before"), "etc.)"),
          tags$li(a("\"Maxima and Minima\"",
                    href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extremes.html",
                    target = "_blank"))
        ),
        "Further possibly helpful pages:",
        tags$ul(
          tags$li(a("Section \"Operators\" in the \"R Language Definition\"",
                    href = "https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Operators",
                    target = "_blank")),
          tags$li(a("\"The R Reference Index\"",
                    href = "https://cran.r-project.org/doc/manuals/r-release/fullrefman.pdf",
                    target = "_blank"))
        )
      )),
      title = "Help pages for R functions",
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  C_pars <- reactive({
    invisible(req(C_stanres()))
    return(brms::variables(C_stanres()$bfit))
  })
  
  observe({
    par_choices <- c("Choose parameter name ..." = "")
    if (!inherits(try(C_pars(), silent = TRUE), "try-error")) {
      par_choices <- c(par_choices, C_pars())
    }
    updateSelectInput(session, "par_sel",
                      choices = par_choices)
  })
  
  observeEvent(input$par_add, {
    req(input$par_sel)
    updateTextInput(session, "cust_text",
                    value = paste0(input$cust_text, "`", input$par_sel, "`"))
  })
  
  C_cust <- reactiveVal(cust_smmry_empty)
  
  # Reset C_cust() when C_stanres() has changed (and also reset
  # `input$cust_text` as well as `input$cust_name`):
  observeEvent(try(C_stanres(), silent = TRUE), {
    C_cust(cust_smmry_empty)
    updateTextInput(session, "cust_text",
                    value = "")
    updateTextInput(session, "cust_name",
                    value = "")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$cust_act, {
    # Check that there is at least one parameter name in `input$cust_text`:
    if (!grepl(paste(paste0("`", C_pars(), "`"), collapse = "|"), input$cust_text)) {
      showNotification(
        paste("Your custom summary has not been calculated since your custom expression did not contain",
              "at least one parameter."),
        duration = NA,
        type = "error"
      )
      return()
    }
    # Check for forbidden code:
    cust_text_valid <- grepl(
      paste0("^(", paste(cust_allow_grp, collapse = "|"), ")*$"),
      gsub(paste(paste0("`", C_pars(), "`"), collapse = "|"), "", input$cust_text)
    )
    if (!cust_text_valid) {
      showNotification(
        "Your custom summary has not been calculated since your custom expression was invalid.",
        duration = NA,
        type = "error"
      )
      return()
    }
    # Check that "C_pars()" contains the correct parameter names:
    if (!identical(C_pars(), colnames(C_draws_mat()))) {
      showNotification(
        "Unexpected parameter names. Please report this.",
        duration = NA,
        type = "error"
      )
      return()
    }
    C_cust(rbind(C_cust(), with(as.data.frame(C_draws_mat()), {
      cust_res <- try(eval(parse(text = input$cust_text)), silent = TRUE)
      if (inherits(cust_res, "try-error")) {
        showNotification(
          "The evaluation of your custom expression failed.",
          duration = NA,
          type = "error"
        )
        return(cust_smmry_empty)
      }
      cust_q <- quantile(cust_res, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
      names(cust_q) <- paste0("Q", sub("%$", "", names(cust_q)))
      names(cust_q)[names(cust_q) == "Q50"] <- "median"
      cust_smmry <- cbind(data.frame("Name" = input$cust_name), as.data.frame(t(cust_q)), data.frame(
        "MAD" = mad(cust_res),
        "mean" = mean(cust_res),
        "SD" = sd(cust_res)
      ))
      return(cust_smmry)
    })))
  })
  
  output$cust_view <- renderTable({
    input$run_stan # Just for graying out.
    ### Only used for making output$cust_view reactive on C_stanres() (so that
    ### output$cust_view grays out while recalculating C_stanres()).
    invisible(try(C_stanres(), silent = TRUE))
    ###
    C_cust()
  })
  
  output$cust_smmry_download <- downloadHandler(
    filename = "shinybrms_custom_summary.csv",
    content = function(file) {
      input$run_stan # Just for graying out.
      write.csv(C_cust(),
                file = file,
                row.names = FALSE)
    }
  )
  
  ### Conditional effects ---------------------------------------------------
  
  # NOTE: suffix "ff" stands for "from fit".
  
  # The "brmsformula" from the fitted model object:
  C_bformula_ff <- reactive({
    invisible(req(C_stanres()))
    return(formula(C_stanres()$bfit))
  })
  
  # A reactive object which will contain the labels of the group terms
  # (excluding those with at least two colons):
  termlabs_PP_grp <- reactiveVal()
  
  observe({
    term_choices <- c("Choose predictor term ..." = "")
    if (!inherits(try(C_bformula_ff(), silent = TRUE), "try-error")) {
      #### Get term labels ------------------------------------------------------
      
      termlabs <- labels(terms(formula(C_bformula_ff())))
      
      #### Pooled effects -------------------------------------------------------
      
      termlabs_CP <- grep("\\|", termlabs, value = TRUE, invert = TRUE)
      termlabs_CP_main <- grep(":", termlabs_CP, value = TRUE, invert = TRUE)
      termlabs_CP_IA <- setdiff(termlabs_CP, termlabs_CP_main)
      termlabs_CP_IA2 <- grep(":.*:", termlabs_CP_IA, value = TRUE, invert = TRUE)
      ### NOTE: unlist() is only needed for the special case
      ### `identical(length(termlabs_CP_IA2), 0L)`:
      termlabs_CP_IA2_rev <- unlist(sapply(strsplit(termlabs_CP_IA2, split = ":"), function(termlabs_CP_IA2_i) {
        return(paste(rev(termlabs_CP_IA2_i), collapse = ":"))
      }))
      ### 
      
      #### Partially pooled effects ---------------------------------------------
      
      termlabs_PP <- setdiff(termlabs, termlabs_CP)
      termlabs_PP_split <- strsplit(termlabs_PP, "[[:blank:]]*\\|[[:blank:]]*")
      stopifnot(all(lengths(termlabs_PP_split) == 2L))
      termlabs_PP_grp_tmp <- sapply(termlabs_PP_split, "[[", 2)
      termlabs_PP_grp_tmp <- grep(":.*:", termlabs_PP_grp_tmp, value = TRUE, invert = TRUE)
      termlabs_PP_grp(termlabs_PP_grp_tmp)
      termlabs_PP_IA <- unlist(lapply(termlabs_PP_split, function(termlabs_PP_i) {
        retermlabs_PP_i <- labels(terms(as.formula(paste("~", termlabs_PP_i[1]))))
        ### May only be used when depending on R >= 4.0.1 (which should probably
        ### be avoided since R 4.0.0 introduced a lot of big changes):
        # return(paste0(retermlabs_PP_i, ":", termlabs_PP_i[2], recycle0 = TRUE))
        ### 
        ### When not depending on R >= 4.0.1:
        if (identical(length(retermlabs_PP_i), 0L)) {
          return(character())
        }
        return(paste0(retermlabs_PP_i, ":", termlabs_PP_i[2]))
        ### 
      }))
      termlabs_PP_IA2 <- grep(":.*:", termlabs_PP_IA, value = TRUE, invert = TRUE)
      ### NOTE: unlist() is only needed for the special case
      ### `identical(length(termlabs_PP_IA2), 0L)`:
      termlabs_PP_IA2_rev <- unlist(sapply(strsplit(termlabs_PP_IA2, split = ":"), function(termlabs_PP_IA2_i) {
        return(paste(rev(termlabs_PP_IA2_i), collapse = ":"))
      }))
      ### 
      
      #### Update choices for input$term_sel ------------------------------------
      
      term_choices <- c(term_choices,
                        termlabs_CP_main, termlabs_CP_IA2, termlabs_CP_IA2_rev,
                        termlabs_PP_grp_tmp, termlabs_PP_IA2, termlabs_PP_IA2_rev)
    } else {
      termlabs_PP_grp(NULL)
    }
    updateSelectInput(session, "term_sel",
                      choices = term_choices)
  })
  
  gg_ceff <- reactive({
    req(input$term_sel, C_stanres())
    re_formula_ceff <- NA
    term_sel_PP <- intersect(input$term_sel, termlabs_PP_grp())
    if (identical(length(term_sel_PP), 1L)) {
      re_formula_ceff <- as.formula(paste("~ (1 |", term_sel_PP, ")"))
    } else if (identical(length(term_sel_PP), 0L) && grepl(":", input$term_sel)) {
      term_sel_split <- strsplit(input$term_sel, split = ":")[[1]]
      stopifnot(length(term_sel_split) <= 2L)
      term_sel_split_PP <- intersect(term_sel_split, termlabs_PP_grp())
      stopifnot(length(term_sel_split_PP) <= 1L)
      if (identical(length(term_sel_split_PP), 1L)) {
        term_sel_split_CP <- setdiff(term_sel_split, term_sel_split_PP)
        stopifnot(identical(length(term_sel_split_CP), 1L))
        re_formula_ceff <- as.formula(paste(
          "~ (1 +", term_sel_split_CP, "|", term_sel_split_PP, ")"
        ))
      }
    }
    ### Not necessary here since there is no sampling taking place (since
    ### argument 're_formula' of brms::conditional_effects() here only contains
    ### the group term which is involved in argument 'effects' (since argument
    ### 're_formula' of brms::conditional_effects() is set to only those
    ### partially pooled effects which are plotted (or also the corresponding
    ### partially pooled intercepts, if partially pooled slopes are plotted))):
    # set.seed(<seed>)
    ### 
    C_ceff <- brms::conditional_effects(
      C_stanres()$bfit,
      effects = input$term_sel,
      re_formula = re_formula_ceff
      ### Not necessary here since there is no sampling taking place (since
      ### argument 're_formula' of brms::conditional_effects() here only
      ### contains the group term which is involved in argument 'effects' (since
      ### argument 're_formula' of brms::conditional_effects() is set to only
      ### those partially pooled effects which are plotted (or also the
      ### corresponding partially pooled intercepts, if partially pooled slopes
      ### are plotted))):
      # sample_new_levels = "gaussian"
      ### 
    )
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      showNotification(
        HTML(paste(
          "Please install package", strong("ggplot2"), "for full plotting",
          "functionality."
        )),
        duration = NA,
        type = "warning"
      )
      C_ceff_plot_list <- plot(C_ceff)
    } else {
      C_ceff_plot_list <- plot(C_ceff, theme = ggplot2::theme_gray(base_size = 16))
    }
    if (length(C_ceff_plot_list) > 1L) {
      showNotification(
        HTML(paste(
          "Function", code("brms:::plot.brms_conditional_effects()"),
          "returned multiple plot objects. Only plotting the first one.",
          "Please report this."
        )),
        duration = NA,
        type = "warning"
      )
    }
    return(C_ceff_plot_list[[1]])
  })
  
  # Only for getting the width in pixels corresponding to argument 'width = "100%"' of plotOutput():
  output$size_aux <- renderPlot({
    req(FALSE)
  })
  
  output$ceff_plot <- renderPlot({
    input$run_stan # Just for graying out.
    gg_ceff()
  },
  width = function() session$clientData$output_size_aux_width,
  height = function() session$clientData$output_size_aux_width * (sqrt(5) - 1) / 2)
  
  output$ceff_download <- downloadHandler(
    filename = function() {
      paste0("shinybrms_cond_eff.", input$ceff_download_sel)
    },
    content = function(file) {
      input$run_stan # Just for graying out.
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        showNotification(
          HTML(paste("Package", strong("ggplot2"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
        return()
      }
      ggplot2::ggsave(
        filename = file,
        plot = gg_ceff(),
        ### In fact, this should be divided by 72 instead of 100, but that gives
        ### a plot which doesn't match the original plot size (in inches) in the
        ### app:
        width = session$clientData$output_size_aux_width / 100,
        height = session$clientData$output_size_aux_width * ((sqrt(5) - 1) / 2) / 100
        ### 
      )
    }
  )
  
  ### shinystan -------------------------------------------------------------
  
  observeEvent(input$act_launch_shinystan, {
    invisible(req(C_stanres()))
    if (requireNamespace("shinystan", quietly = TRUE)) {
      if (requireNamespace("callr", quietly = TRUE)) {
        # The browser for "shinystan":
        shinystan_browser <- getOption("shinybrms.shinystan_browser",
                                       getOption("browser"))
        if (is.function(shinystan_browser) &&
            any(grepl("rs_browseURL|rs_shinyviewer", as.character(body(shinystan_browser))))) {
          # In this case, "shinystan_browser" cannot be used (at least not without requiring the
          # user to perform some major modifications to the initialization of the R session), so use
          # the default browser stored in the environment variable "R_BROWSER":
          shinystan_browser <- Sys.getenv("R_BROWSER")
          if (identical(.Platform$OS.type, "windows") &&
              identical(shinystan_browser, "")) {
            shinystan_browser <- NULL
          }
        }
        
        # Get the seed for drawing from the posterior predictive distribution:
        seed_PPD_tmp <- input$seed_PPD
        if (is.na(seed_PPD_tmp)) {
          seed_PPD_tmp <- NULL
        }
        
        # Call "shinystan" from an external R process (needed to allow opening another "shiny" app
        # (here "shinystan") from within the current "shiny" app (here "shinybrms")):
        callr::r(
          function(bfit_obj, browser_callr, seed_callr) {
            browser_callr_orig <- options(browser = browser_callr)
            assign("y", brms::get_y(bfit_obj), envir = .GlobalEnv)
            if (!is.vector(y)) assign("y", as.vector(y), envir = .GlobalEnv)
            set.seed(seed_callr)
            assign("y_rep", brms::posterior_predict(bfit_obj), envir = .GlobalEnv)
            shinystan::launch_shinystan(bfit_obj,
                                        rstudio = FALSE)
            options(browser = browser_callr_orig$browser)
            return(invisible(TRUE))
          },
          args = list(bfit_obj = C_stanres()$bfit,
                      browser_callr = shinystan_browser,
                      seed_callr = seed_PPD_tmp)
        )
      } else {
        showNotification(
          HTML(paste("Package", strong("callr"), "needed. Please install it.")),
          duration = NA,
          type = "error"
        )
      }
    } else {
      showNotification(
        HTML(paste("Package", strong("shinystan"), "needed. Please install it.")),
        duration = NA,
        type = "error"
      )
    }
  })
  
  ## Quit app ---------------------------------------------------------------
  
  observe({
    if (identical(input$navbar_ID, "quit_app")) {
      if (exists("lc_collate_orig")) {
        if (identical(length(lc_collate_orig), 1L) &&
            is.character(lc_collate_orig) &&
            is.vector(lc_collate_orig)) {
          Sys.setlocale("LC_COLLATE", lc_collate_orig)
        } else {
          Sys.setlocale("LC_COLLATE", "")
        }
      }
      stopApp()
    }
  })
  
  session$onSessionEnded(
    function() {
      if (exists("lc_collate_orig")) {
        if (identical(length(lc_collate_orig), 1L) &&
            is.character(lc_collate_orig) &&
            is.vector(lc_collate_orig)) {
          Sys.setlocale("LC_COLLATE", lc_collate_orig)
        } else {
          Sys.setlocale("LC_COLLATE", "")
        }
      }
      stopApp()
    }
  )
  
}

# Call to shinyApp() ------------------------------------------------------

shinyApp(ui = ui, server = server)
