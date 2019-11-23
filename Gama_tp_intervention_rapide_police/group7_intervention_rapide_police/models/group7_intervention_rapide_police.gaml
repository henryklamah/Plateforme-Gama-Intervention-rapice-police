/***
* Name: group7interventionrapidepolice
* Author: okimb
* Description: Un modele GAMA de simulation rapide de police
* Tags: Tag1, Tag2, TagN
***/

model group7interventionrapidepolice

global {
	/** Insert the global definitions, variables and actions here */
	file espace_execution_shapefile <- file("../includes/limites.shp");
	file croisement_shapefile <- file("../includes/noeuds.shp");
	file poste_police_shapefile <- file("../includes/postespolices.shp");
	file routes_shapefile <- file("../includes/routes.shp");
	geometry shape <- envelope(espace_execution_shapefile);
	int nb_polices <- int(nb_population/10);
	int nb_population parameter: 'nb_population' <- 40 min: 10 max:40;
	float speed_veh <-0.5 parameter:"vitesse des voiture";
	list temp_interv<-[0];
	
	
	float proba_agressivite<- 0.6 parameter:"Probabilité agressif";
	graph le_graph;
	int max_temp<-50 parameter:"Temps max d attente de cas";
	int pas_de_cas_temps <-0;
	int max_vehicule_poste <-2 parameter:"Max voiture par poste";
	
	
	list meurt<-[];
	list norm<-[];
	list agress<-[];
	
	init{
		// creation des postes Polices
		create postes_polices from: poste_police_shapefile;	
		// création de personne
		create civil number:nb_population{
			
		}
			
		// création de l'agent route
		create routes from:routes_shapefile;
		le_graph <- as_edge_graph(routes);
		
	// affectation des personnes corps de la police dans les differents postes de police
				
		ask (nb_polices among (list(personne))){
			post <- one_of(postes_polices);
			location <- any_location_in(post);
		}
	
	}
	reflex actualiser_stat{
		norm<-civil where (!each.agressif);
	    agress<-civil where (each.agressif);
		meurt<-civil where (each.meurt);
	}
	reflex arret { // s'il n'y a pas d'intervention pendant un moment donnée, alors arreter la simulation
		list postes <- list(postes_polices);
		loop poste over:postes{
			if(poste.vehic_sent!=0){
				pas_de_cas_temps<-0;
				break;
			}else{
				pas_de_cas_temps<-pas_de_cas_temps+1;
			}
		}
		if(max_temp=pas_de_cas_temps){
			do pause;
		}
		
		
	}
	
}
// inialialiation de poste de police
species postes_polices{
	int cas<-0;
	int vehic_count<-max_vehicule_poste;
	int vehic_sent<-0;
	point cas_signale<-nil;
	reflex envoi_vehicule when:(cas_signale != nil and vehic_sent<vehic_count){
		write "Police: recoit signel";
		create vehicule number:1{ // creer le signal et envoyer au lieu du meurtre
			self.lieu_intervention<-myself.cas_signale;
			self.target<-lieu_intervention;
			myself.cas_signale<-nil;
			self.location<-myself.location+2; // pour eviter de creer la voiture au meme lieu que poste_police
			self.poste<-myself;
		}
		vehic_sent<-vehic_sent+1;
	}
	
	
	aspect base{ // dessin du poste police
		draw square(10) color:#blue;
		draw square(10) color:#white at:{location.x,location.y+5};
		draw square(10) color:#red at:{location.x,location.y+10}; 
	}
}
// initialialisation de l'agent personne
species personne{
	rgb couleur <- #green;
	float taille <- 5.0;
	bool est_agressee <- false;
	bool est_mort <- false;
	float rayon_emission <- 200.0 #m;
	postes_polices post <- nil;
	int point_de_vie <-20+rnd(30);
	
	
	list<postes_polices> liste_poste <- nil;
	
	
	
	
	
	aspect base{
		draw circle(taille) color:color;
	}
	
		
}

// initialisation de l'agent police


species civil skills:[moving] parent:personne{
	bool agressif <- rnd(1.0)>proba_agressivite;
	bool agressee <-false;
	int force_frappe<-5+rnd(3); // puissance_de_frappe
	bool meurt <-false;
	bool signale<-false;
	
	int corps_trouve<-0;
	
	reflex vivre{ // pour permettre de gerer la mort:::: si point de vie <=0 alors on le declare mort
		if(point_de_vie<=0){
			meurt<-true;
		}
	}
	action envoi_singal_propre{ // rechercher poste de police et signaler au plus proche		
		liste_poste <- (postes_polices at_distance(rayon_emission));
		ask liste_poste with_min_of(each distance_to self){
			self.cas_signale<-myself.location;
		}
	}
	action envoi_singal_meurtre{ // rechercher poste de police et signaler au plus proche		
		liste_poste <- (postes_polices at_distance(rayon_emission));
		ask liste_poste with_min_of(each distance_to self){
			self.cas_signale<-one_of(civil where (each.meurt and !each.signale)).location;
		}
	}
	reflex agresser when:((agressif and !meurt) ){ // reflex agresser est actif pour les agressif et qui ne sont pas encore mort 
	// si quelqu'un normal est agressee alors il devient agressif apres avoir signaler à la police
		civil a_agresser<-nil;
		ask civil with_min_of(each distance_to self ){
			a_agresser<-self;
		}
		if(a_agresser!=nil or !meurt){
			do goto target:a_agresser.location;
			if(self distance_to a_agresser<0.5){
			do envoi_singal_propre;
			a_agresser.point_de_vie<-a_agresser.point_de_vie-force_frappe; // l'agression se fait en retranchant le point de vie de la personne agressée pr la force de frappe
			a_agresser.agressif<-true;
				write "agressee: attaquée";
			}
		}else{
			do wander amplitude:50.0;
		}
	}
	reflex observer when:(!agressif){ // si quelqu'un n'est pas agressif alors quand il voit une meurtre il signale à la police
		corps_trouve<- length(list(civil where (each.meurt and !signale)));
	}
	reflex temoigner when:(corps_trouve>1){ // pour singaler à la police quand il corps_trouvé >0
		do envoi_singal_meurtre;
		write "Temoin: corps trouvé";
	}
	
	reflex errer when:(!meurt){ // s'il n'est pas encore mort alors il erre dans l'espace
		do wander;
	}
	aspect base{
		color<-agressif?#red:#green; // color différent pour chaque type
		color<-meurt?#black:color;
		draw circle(taille) color:color;
	}	
}
species routes {
	rgb couleur <- #gray;
	aspect base{
		draw shape + 5 color:couleur;
	}
}

// initialisation de l'agent vehicule
species vehicule skills:[advanced_driving]{
	
	//rgb couleur_vehicule<-rgb(rnd(255), rnd(255),rnd(255));
	rgb color;
	float size;
	point target <- nil;
	float vehicule_speed <- rnd(100.0)+5.0;
	point lieu_intervention<-nil;
	int temp<-0;
	
	
	postes_polices poste <- nil;
	point dest <- nil;
	
	reflex move when: target != nil{
		do goto target:target on:le_graph speed:speed_veh;
		temp<-temp+1;
		if(target distance_to self<10 and target=poste.location){ // s'il est tout près de la destination alors on considere qu'il est déjà arrivée en meme temps
			location<-target;
		}
		if(location=target and location = lieu_intervention){ // s'il arrive à destination et que la destination est le lieu d'intervention alors il retourne au poste de police
			temp_interv<<temp;
			set target <- poste.location;
			ask civil at_distance(5){ // ramener tout le monde aux alentours du meurtre les temoins, le coupable, etc
				do die;
			}
		}
		if(location=target and self distance_to(poste)<10){ // s'il arrive au poste de police alors il augmente la voiture disponible du poste
			ask poste{
				vehic_sent<-vehic_sent-1;
			}
			self.color<-#white;
			do die; // il disparait apres la mission
		}
	}
	
	
	// aspect de l'agent
	aspect base {
		draw square(10) color: #yellow; // 
	}
	
}


experiment group7interventionrapidepolice type: gui {
	/** Insert here the definition of the input and output of the model */
	output {   
			display intervension type: opengl{
				species civil aspect:base;
            species routes aspect:base;
           // species vehicule aspect:base;
            species postes_polices aspect:base; 
            species personne aspect:base;  
            species vehicule aspect:base;       
        }
        
         //affichage de la statistique
		display Series_Stat_Display{
            	chart "Statistique d'intervention" type: series {
            		
                data "Temps moyenne d'intervention" value: mean(temp_interv) color: #blue;
                data "Nombre de mort" value: length(meurt) color: #black;
                
            }
            
            
        }
        display Series_Stat_agresseur{
        	chart "Statistique agresseur" type: pie {
            	            		
                data "agresseur" value: length(agress) color: #red; 
                data "normal" value: length(norm) color: #green;
                
            }
        }
        }
}