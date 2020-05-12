sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
    tags$form(class = "well", ...),
    out
  )
}


ui <- fluidPage(
	shinyjs::useShinyjs(),

	titlePanel(title=div(img(src="_coronavirus.png", height = 80, width = 80), HTML("COVID-19 contact tracing app simulator"))),
	sidebarLayout(position = "left",
		sidebarPanel2(fluid = FALSE,

			h4("Effect of the contact tracing app:"),
			radioButtons("app_use", label = "People using the app:", 
                                choices = c('0%'=0.0, '25%'=0.25, '50%'=0.50, '75%'=0.75), selected = 0.0, inline=T),
			br(),
			h4("Relevant demographic and epidemiological parameters:"),
			radioButtons("density", label = "Population density (ppl/km²):", 
                                choices = c(1, 5, 20, 'variable (source: Italian demography)'='NA'), selected = 'NA', inline=T),
			radioButtons("contact_rate", label = "Contact rate (*):", 
                                choices = c(7.5, 10, 14.8,'specify velocity'='NA'), selected = 'NA', inline=T),
			radioButtons("velocity", label = "Velocity (km/day) (source: Spanish mobility):",
                                choices = c(1.5, 2.5, 5.2), selected = 1.5, inline=T),
			out = h5(HTML("(*) C.J. Rhodes & R.M. Anderson. <i>Mathematical Biosciences</i> 216.1 (2008), pp. 56-62. DOI: 10.1016/j.mbs.2008.08"))

		),
		mainPanel(
			tabsetPanel(type = "tabs",
				tabPanel("Simulations",
					HTML(paste(
						HTML("<br/>"),
						h4(HTML("This R Shiny web application allows to visualize the effect of a contact tracing 
						app to control SARS-CoV-2 epidemics. In particular, we show the results of
						a <b>proof-of-concept study</b> in which a basic epidemiological model, accounting for asymptomatic infection
						and population density, is informed about the use of a contact tracing app. Different <b>illustrative scenarios</b> can be
						visualized by using the control panel on the left-hand side. A tutorial and details on the model are available 
						in the corresponding tab panels.")),sep='')),
					uiOutput('image')),
				tabPanel("Tutorial",
					HTML(paste(
						HTML("<br/>"),
						h4(HTML("Each radio button on the left-hand side controls a selection of parameters related of the model. In particular,
							results of simulations with different percentages of the population using the contact tracing app
							can be visualized through the <b><i>Effect of the contact tracing app</i></b> panel. The curve 
							corresponding to the selected percentage will be highlighted in the visualization.")),
						img(HTML('<center><img src="_01.png"></center>')),
						HTML("<br/>"),
						h4(HTML("The visualization shows two plots representing the results of the simulations concerning the dynamics 
							of the population in the I (Infectious) and QI (Quarantined Infectious) compartments (<font color='red'><b>red curves</b></font> on the left)
							as well as the dynamics of the overall mortality (<b>black curves</b> on the right).")),
						img(HTML('<center><img src="_02.png"></center>')),
						h4(HTML("The user can explore a number of illustrative scenarios by selecting combinations of parameters in the 
							<b><i>Relevant demographic and epidemiological parameters</i></b> panel. This selection of parameters include:")),
						HTML("<ul>
							<li><h4><b>Population density:</b> 
								it can be constant (1, 5 or 20 ppl/km², reflecting small, medium and large sized cities) or variable
								(based on the actual Italian demography) for each node of the model.</h4></li>
							<li><h4><b>Contact rate:</b>
								calculated based on <a href='https://www.sciencedirect.com/science/article/abs/pii/S0025556408001260?via%3Dihub'>
								Rhodes & Anderson (2008)</a>, it can be pre-defined (7.5, 10, or 14.8, reflecting few, medium and many contacts) or 
								depending on a specific velocity (<i>specifiy velocity</i>).</h4></li>
							<li><h4><b>Velocity:</b>
								it can be increased or reduced in a range (1.5, 2.5 or 5.2 km/day) based on estimations of the mobility
								in Spain before and after the lockdown (14/03/2020).</h4></li>
							</ul>"),
						h4(HTML("As described in the <i>Model</i> tab panel, our model takes into account many other parameters whose values 
							are based on evidence in the literature. Nevertheless, it is important to stress that the current knowledge about SARS-CoV-2
							infection and transmission is still characterized by many uncertainties. For this reason, we reckon that
							a user-friendly platform with relevant control options is ideal to convery the main objectives of our study.")),
						HTML("<br/>"),
						sep=''))
					),
				tabPanel("Model",
					HTML(paste(
							HTML("<br/>"),
							h4(HTML("We built a Susceptible-Infectious-Recovered (<b>SIR</b>) model with additional asymptomatic (<b>A</b>) and pre-symptomatic (<b>P</b>)
									compartments and corresponding quarantine (<b>Q*</b>) compartments. In the following, we describe the model parameters
									and the bibiographic sources. The structure of the model is shown below:")),
							HTML("<br/>"),
							img(HTML('<center><img src="_03.png"></center>')),
							HTML("<br/>"),
							h4(HTML("S individuals move to the A or P compartments with probabilities <i>p</i><sub>a</sub> = 0.4 [1,2] and
									<i>p</i><sub>i</sub> = 1 - <i>p</i><sub>a</sub>, respectively. P subjects move to I compartment after an incubation 
									period <i>&tau;</i><sub>i</sub> of 5.1 days [3].")),
							h4(HTML("The contact tracing app recommends voluntary quarantine to people
									who had been in contact with a symptomatic case. As a <i>J</i> fraction of symptomatic cases are identified and quarantined
									for a period <i>&tau;</i><sub>q</sub> of 15 days (based on WHO recommended 14-day quarantine), <i>J</i><i>j</i><sup>2</sup> is 
									the proportion of contacts that are sucessfully contained, when a <i>j</i> fraction of the population uses the app and self-quarantine")),
							h4(HTML("The contact rate <i>C</i> is parametrized to a population with density <i>&rho;</i> moving with velocity <i>v</i> as in Rhodes & Andreson (2008) [4], 
								with infection transmission distance <i>R</i> equals to 1 meter [4]:")),
							img(HTML('<center><img src="_04.png"></center>')),
							h4(HTML("The model also includes the following additional parameters:")),
							HTML("<ul>
									<li><h4>A and P subjects are less infectious than S subjects by a factor <i>f</i> = 0.10 [1].</li></h4>
									<li><h4>P subjects are already infectious <i>&tau;</i><sub>d</sub> = 2 days before symptom onset [2].</li></h4>
									<li><h4>Conctact rate <i>C</i> is reduced by a factor <i>q</i> = 0.1 during quarantine.</li></h4>
									<li><h4>The probability of transmission in a single contact <i>&mu;</i> = 0.1 [1].</li></h4>
									<li><h4>The overall mortality of symptomatic cases is 1%.</li></h4>
									</ul>"),
							HTML("<br/>"),
							h4(HTML("<font size=2>[1] Ferretti et al. <i>Science</i> (2020). DOI: 10.1126/science.abb6936</font>")),
							h4(HTML("<font size=2>[2] He et al. <i>Nature medicine</i> (2020), DOI: 10.1038/s41591-020-0869-5</font>")),
							h4(HTML("<font size=2>[3] Lauer et al. <i>Annals of Internal Medicine</i> (2020). DOI: 10.7326/M20-0504</font>")),
							h4(HTML("<font size=2>[4] Rhodes and Anderson. <i>Mathematical Biosciences</i> (2008). DOI: 10.1016/j.mbs.2008.08</font>")),
							h4(HTML("<font size=2>[5] Wells. <i>American Journal of Epidemiology</i> (1934). DOI: 10.1093/oxfordjournals.aje.a118097</font>")),

						sep=''))

					),
				tabPanel("About", 
					HTML(paste(
						HTML("<br/>"),
						h4("The model was developed by Alberto Ferrari (Fondazione per la Ricerca Ospedale Maggiore di Bergamo)."),
						HTML("<br/>"),
						h4("The research team that contributed to the work includes:"),
						HTML("<br/>"),
    					h4(HTML('&emsp;'),"Enrico Santus (Bayer pharmaceuticals)"),
    					h4(HTML('&emsp;'),"Davide Cirillo (Barcelona Supercomputing Center)"),
    					h4(HTML('&emsp;'),"Miguel Ponce de León (Barcelona Supercomputing Center)"),
    					h4(HTML('&emsp;'),"Alfonso Valencia (Barcelona Supercomputing Center)"),
    					h4(HTML('&emsp;'),"Nicola Marino (Women's Brain Project)"),
    					h4(HTML('&emsp;'),"Maria Teresa Ferretti (Women's Brain Project)"),
    					h4(HTML('&emsp;'),"Antonella Santuccione Chada (Women's Brain Project)"),
    					h4(HTML('&emsp;'),"Nikolaos Mavridis (Massachussets Institute of Technology)"),sep=""))
					)
				)
			)
		)
)