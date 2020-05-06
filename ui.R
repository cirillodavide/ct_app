ui <- fluidPage(

	titlePanel(title=div(img(src="coronavirus.png", height = 80, width = 80), HTML("COVID-19 contact-tracing app simulator"))),
	sidebarLayout(position = "left",
		sidebarPanel(

			sliderInput("Cs", "Contact rate (C):", min = 0.0, max = 15.0, value = 1.5, step = 0.1),
			sliderInput("ti", "Incubation period (days) (ti):", min = 0.0, max = 10.0, value = 5.1, step = 0.1),
			sliderInput("tq", "Quarantine duration (days) (tq):", min = 0.0, max = 30.0, value = 15.0, step = 0.1),
			sliderInput("f", "Relative infectiousness of a- and pre-symptomatic compared to symptomatic (f):", min = 0.0, max = 1.0, value = 0.1, step = 0.1),
			sliderInput("Ptrasm", "Probability of transmission in a single contact (mu):", min = 0.0, max = 1.0, value = 0.10, step = 0.05),
			sliderInput("R", "Maximum distance to qualify a contact (meters) (R):", min = 0.0, max = 5.0, value = 1.0, step = 0.5),
			sliderInput("i_prop", "S->P probability (pi):", min = 0.0, max = 1.0, value = 0.6, step = 0.1),
			sliderInput("j", "Proportion of app users in the population (j):", min = 0.0, max = 1.0, value = 0.0, step = 0.05),
			sliderInput("q_", "Contact rate reduction for quarantied app users (q):", min = 0.0, max = 1.0, value = 0.1, step = 0.1),
			sliderInput("J", "Fraction of symptomatic cases is identified and quarantied (J):", min = 0.0, max = 1.0, value = 0.75, step = 0.1),

			sliderInput("DENS", "DENS:", min = 0.0, max = 1.0, value = 0.0, step = 0.1),
			sliderInput("t_to_death", "t_to_death:", min = 0, max = 100, value = 8, step = 1),
			sliderInput("td", "td:", min = 0, max = 10, value = 2, step = 1)

			),
		mainPanel(
			actionButton("updateButton", label = "Run simulation"),
			plotOutput('plot')
			)
		)
	)