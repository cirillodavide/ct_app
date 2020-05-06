require(SimInf)
require(dplyr)
require(glue)
require(rapportools)

# data preparation
Prov_pop=read.csv("data/Popolazione.csv", sep=";")
Prov_pop$Provincia=sapply(Prov_pop$Provincia, trim.space, what="both")
Prov_pop$Ettari=Prov_pop$Superficie..kmq./0.01
Prov_pop$Density=Prov_pop$Residenti/(Prov_pop$Superficie..kmq.*1000)

Data <-Prov_pop
Data$Density=Data$Residenti/Data$Ettari

Tot_Dens=(sum(Prov_pop$Residenti)/sum(Prov_pop$Ettari))

# CONSTANTS:

transitions <- 
  c("S -> (PI*(1-pa)*            c0*Density*S*(I)*(1-j*j)+
            PI*(1-pa)*f*   c0*Density*S*(A)+
            PI*(1-pa)*f*   c0*Density*S*(P)*(1-j*j)+
              PI*(1-pa)*             c0*Density*S*(QI)*q_0+
                PI*(1-pa)*f*  c0*Density*S*(QP+QA)*q_)
                /(S+I+R+A+QA+QP+QS+QI+P) 
                  -> P",
    
    "S -> (PI*pa*             c0*Density*S*(I)*(1-j*j)+
            PI*pa*f*   c0*Density*S*(A)+
            PI*pa*f*   c0*Density*S*(P)*(1-j*j)+
              PI*pa*             c0*Density*S*(QI)*q_0+
                PI*pa*f*  c0*Density*S*(QP+QA)*q_)
                /(S+I+R+A+QA+QP+QS+QI+P) 
                  -> A+Acum", 
      
    "S -> eff*test*(1-(PI*(1-pa))-PI*pa)*  c0*Density*S*(I)*J*j*j/
              (S+I+R+A+QA+QP+QS+QI+P) -> QS+Qcum+T",
    "S -> eff*PI*(1-pa)*           c0*Density*S*(I)*J*j*j/
              (S+I+R+A+QA+QP+QS+QI+P) -> QP+QIcum+Qcum",
    "S -> eff*PI*    pa*           c0*Density*S*(I)*J*j*j/
              (S+I+R+A+QA+QP+QS+QI+P) -> QA+Qcum",

    
    "P -> P/k -> I+Icum", 
    "P -> eff*c0*Density*P*(I)*J*j*j/(S+I+R+A+QA+QP+QS+QI+P) -> QP+QIcum+Qcum",
 
    "I -> J*I/td -> QI+Qcum",
    "I -> I/th -> R", 
    "I -> m*I -> D",
    
    "A -> A/th -> R", 
    "A -> eff*c0*Density*A*(I)*J*j*j/(S+I+R+A+QA+QP+QS+QI+P) -> QA+Qcum",
    
    "QI -> QI/th -> R", 
    "QI -> m*QI -> D",
    
    "QP -> QP/k -> QI+Icum", 
    
    "QA -> QA/th -> R", 
    
    "QS -> QS/tq -> S")


compartments <- c("S", "I" ,"Icum", "R", "A" ,"Acum","QA", "QP","QS", 
                  "QI","Qcum", "P", "D", "T","QIcum")


server <- function(input, output, session) {

	v <- reactiveValues()

	observeEvent(input$updateButton,{

	showModal(modalDialog("Please wait...", footer=NULL))

	# internal controller
	
	EFF = 1
	TEST = 1
	switch = 1

	#Parameters

	nsim=1
	Cs = input$Cs
	span = c(seq(1,50, by=2), seq(51,100, by=5), seq(101,600, by=20))
	j = input$j

	t_to_death = input$t_to_death
	i_prop = input$i_prop
	pa = 1-i_prop
	f = input$f
	DENS = input$DENS
	R = input$R
	x = 8*R*Cs/pi
	C0 = ifelse(DENS!=1, x, Cs)
	Ptrasm = input$Ptrasm
	td = input$td
	tq = input$tq
	ti = input$ti - td
	J = input$J
	q_ = input$q_
	q_0 = 0

	nodes = rep(1:nrow(Prov_pop), nsim)
	sim = rep(1:nsim, each=nrow(Prov_pop))
	n <- nsim*nrow(Prov_pop)

	startP = 100
	u0 <- data.frame(S = rep(Prov_pop$Residenti, nsim), I = rep(startP*0.2, n), Icum = rep(0, n), R = rep(0, n),
                 	P = rep(startP*0.8,n), QA = (rep(0,n)), QP = (rep(0,n)), QS = (rep(0,n)),
                 	QI = (rep(0,n)),Qcum = (rep(0,n)), QIcum = (rep(0,n)), A = (rep(0,n)), Acum = (rep(0,n)),
                 	D = (rep(0,n)),T = (rep(0,n)))

	node_data=NULL
		if (DENS!=1) {
  			node_data=data.frame(rep(Prov_pop$Density, nsim), rep(Prov_pop$Ettari, nsim))
  			names(node_data)=c("Density","Ettari")
		} else {
  			node_data=data.frame(rep(rep(1.0, nrow(Prov_pop)), nsim), rep(Prov_pop$Ettari, nsim))
  			names(node_data)=c("Density","Ettari")
		}

	j = j*switch
	J = J*switch

	model <- mparse(transitions = transitions, compartments = compartments,
            		gdata = c(c0=C0,
                    			PI = Ptrasm, pa=pa, 
                    			j=j, th = 10+td, tq=tq, test=TEST, 
                    			k=ti, td=td, J=J, eff=EFF, q_=q_, q_0=q_0,
                    			m=0.01/t_to_death, f=f),
            		ldata = node_data, 
      				#E=E, N=N, events=rbind(lockdown, release),
            		u0 = u0, tspan = span)

	set.seed(123)
	result <- run(model = model)
	data_out=trajectory(model = result)

	data_out=cbind(nodes, sim, data_out)
	data_out_G_app_=aggregate(cbind( S ,    I ,    R,     A  ,  
                            	Acum,  QA  ,  QP   , QS  ,  QI ,   Qcum,  P ,    D,    
                            	Icum,  T, Qcum)~time+sim, data=data_out, FUN=sum)

	data_out_G_app=aggregate(cbind( S ,    I ,    R,     A  ,  
                           		Acum,  QA  ,  QP   , QS  ,  QI ,   Qcum,  P ,    D,    
                           		Icum,  T, Qcum)~time, data=data_out_G_app_, FUN=mean)

	data_std=aggregate(cbind( S ,    I ,    R,     A  ,  
                     		Acum,  QA  ,  QP   , QS  ,  QI ,   Qcum,  P ,    D,    
                     		Icum,  T, Qcum)~time, data=data_out_G_app_, FUN=sd)


	v$lst <- list(data_out_G_app$time,data_out_G_app$I+data_out_G_app$QI,data_out_G_app$D)
    
    removeModal()
    })

	output$plot <- renderPlot({
   				if (is.null(v$lst)) return()
   				lst <- v$lst
    			options(scipen=999)
				plot(lst[[1]], lst[[2]], type="l", col="red", xlab="Time (days)", ylab="Subjects", lwd=4)
				lines(lst[[1]], lst[[3]], col="black", lwd=4)
  	})


}