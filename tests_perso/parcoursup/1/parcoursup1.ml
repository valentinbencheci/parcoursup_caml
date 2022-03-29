(* ###################### - DESCRIPTION TEST - ###################### *)
(* Dans ce test, nous vérifions comment les candidats refusent 
consécutivement les places à l'université. Ainsi on peut voir si les 
algorithmes "refuser" et "nouveau_jour" fonctionnent correctement. *)
(* ################################################################## *)

open Test_parcoursup_utils

let session = nouvelle_session ()

let () = 
  ajoute_candidat session ~nom_candidat:"Adam";
  ajoute_candidat session ~nom_candidat:"Olivie";
  ajoute_candidat session ~nom_candidat:"Marie";
  ajoute_candidat session ~nom_candidat:"Pierre";
  ajoute_candidat session ~nom_candidat:"Alan";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:2;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Paris" ~capacite:1;
  ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Adam" 
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Olivie" 
    ~nom_formation:"Université de Toulon";
  ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Marie" 
    ~nom_formation:"Université de Paris";
  ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Pierre" 
    ~nom_formation:"Université de Paris";
  ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Alan" 
    ~nom_formation:"Université de Paris";
  ajoute_commission session 
    ~nom_formation:"Université Côte d'Azur" 
    ["Adam";"Olivie";"Marie";"Pierre";"Alan"]; 
  ajoute_commission session 
    ~nom_formation:"Université de Toulon" 
    ["Adam";"Olivie";"Marie";"Pierre";"Alan"]; 
  ajoute_commission session 
    ~nom_formation:"Université de Paris" 
    ["Adam";"Olivie";"Marie";"Pierre";"Alan"]; 
    
  reunit_commissions session;

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Olivie";
  affiche_propositions_en_attente session "Olivie";
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";
  affiche_voeux_en_attente session "Alan";
  affiche_propositions_en_attente session "Alan";

  renonce session 
    ~nom_candidat:"Marie" 
    ~nom_formation:"Université de Paris";
  nouveau_jour session;
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";
  affiche_voeux_en_attente session "Alan";
  affiche_propositions_en_attente session "Alan";

  renonce session 
    ~nom_candidat:"Pierre" 
    ~nom_formation:"Université de Paris";
  nouveau_jour session;
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";
  affiche_voeux_en_attente session "Alan";
  affiche_propositions_en_attente session "Alan";

  renonce session 
    ~nom_candidat:"Alan" 
    ~nom_formation:"Université de Paris";
  nouveau_jour session;
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";
  affiche_voeux_en_attente session "Alan";
  affiche_propositions_en_attente session "Alan";

  renonce session 
    ~nom_candidat:"Alan" 
    ~nom_formation:"Université de Paris";
  nouveau_jour session;
  affiche_voeux_en_attente session "Marie";
  affiche_propositions_en_attente session "Marie";
  affiche_voeux_en_attente session "Pierre";
  affiche_propositions_en_attente session "Pierre";
  affiche_voeux_en_attente session "Alan";
  affiche_propositions_en_attente session "Alan";