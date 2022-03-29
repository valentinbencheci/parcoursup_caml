(* ###################### - DESCRIPTION TEST - ###################### *)
(* On regarde l'algorithme quand plusiers candidats refusent 
simultanement *)
(* ################################################################## *)

open Test_parcoursup_utils

let session = nouvelle_session ()

let () = 
  ajoute_candidat session ~nom_candidat:"Adam";
  ajoute_candidat session ~nom_candidat:"Olivier";
  ajoute_candidat session ~nom_candidat:"Marie";
  ajoute_candidat session ~nom_candidat:"Pierre";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:7;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:2;
  ajoute_formation session ~nom_formation:"Université de Paris" ~capacite:1;
  ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Adam" 
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Olivier" 
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session 
    ~rang_repondeur:(Some 2) 
    ~nom_candidat:"Marie" 
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Pierre" 
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session 
    ~rang_repondeur:(Some 2) 
    ~nom_candidat:"Marie" 
    ~nom_formation:"Université de Paris";
  ajoute_voeu session 
    ~rang_repondeur:(Some 5) 
    ~nom_candidat:"Adam" 
    ~nom_formation:"Université de Paris";
  ajoute_voeu session 
    ~rang_repondeur:(Some 5) 
    ~nom_candidat:"Marie" 
    ~nom_formation:"Université de Toulon";

  ajoute_commission session 
    ~nom_formation:"Université Côte d'Azur" 
    ["Adam";"Olivier";"Marie";"Pierre"]; 
  ajoute_commission session 
    ~nom_formation:"Université de Toulon" 
    ["Adam";"Olivier";"Marie";"Pierre"];  
  ajoute_commission session 
    ~nom_formation:"Université de Paris" 
    ["Adam";"Olivier";"Marie";"Pierre"]; 
    
  reunit_commissions session;

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Olivier";
  affiche_propositions_en_attente session "Olivier";
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";

  renonce session 
    ~nom_candidat:"Adam" 
    ~nom_formation:"Université Côte d'Azur";
  renonce session 
    ~nom_candidat:"Marie" 
    ~nom_formation:"Université Côte d'Azur";

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Olivier";
  affiche_propositions_en_attente session "Olivier";
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";