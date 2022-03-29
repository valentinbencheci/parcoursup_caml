(* ###################### - DESCRIPTION TEST - ###################### *)
(* On verifie comment fonctionne l'algo quand un candidat a une grande 
liste d'attente et puis, tous les formations lui sont disponibles. 
En plus, on verifie ce que se passe quand Emma refuse une formation 
qu'elle n'a pas choisi.*)
(* ################################################################## *)

open Test_parcoursup_utils

let session = nouvelle_session ()

let () =
  ajoute_candidat session ~nom_candidat:"Adam";
  ajoute_candidat session ~nom_candidat:"Leo";
  ajoute_candidat session ~nom_candidat:"Raphael";
  ajoute_candidat session ~nom_candidat:"Louise";
  ajoute_candidat session ~nom_candidat:"Alice";
  ajoute_candidat session ~nom_candidat:"Emma";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Lyon" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Paris" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Grenoble" ~capacite:1;

  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Adam"
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Leo"
    ~nom_formation:"Université de Toulon";
  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Raphael"
    ~nom_formation:"Université de Lyon";
  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Louise"
    ~nom_formation:"Université de Paris";
  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Alice"
    ~nom_formation:"Université de Grenoble";

  ajoute_voeu session
    ~rang_repondeur:(Some 5)
    ~nom_candidat:"Emma"
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session
    ~rang_repondeur:(None)
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Toulon";
  ajoute_voeu session
    ~rang_repondeur:(Some 0)
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Lyon";
  ajoute_voeu session
    ~rang_repondeur:(Some 0)
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Paris";
  ajoute_voeu session
    ~rang_repondeur:(None)
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Grenoble";

  ajoute_commission session
    ~nom_formation:"Université Côte d'Azur"
    ["Adam";"Leo";"Emma";"Louise";"Alice";"Raphael"];
  ajoute_commission session
    ~nom_formation:"Université de Toulon"
    ["Adam";"Leo";"Emma";"Louise";"Alice";"Raphael"];
  ajoute_commission session
    ~nom_formation:"Université de Lyon"
    ["Adam";"Leo";"Emma";"Louise";"Alice";"Raphael"];
  ajoute_commission session
    ~nom_formation:"Université de Paris"
    ["Adam";"Leo";"Emma";"Louise";"Alice";"Raphael"];
  ajoute_commission session
    ~nom_formation:"Université de Grenoble"
    ["Adam";"Leo";"Emma";"Louise";"Alice";"Raphael"];

  reunit_commissions session;

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";
  affiche_voeux_en_attente session "Raphael";
  affiche_propositions_en_attente session "Raphael";
  affiche_voeux_en_attente session "Louise";
  affiche_propositions_en_attente session "Louise";
  affiche_voeux_en_attente session "Alice";
  affiche_propositions_en_attente session "Alice";
  affiche_voeux_en_attente session "Emma";
  affiche_propositions_en_attente session "Emma";

  renonce session
    ~nom_candidat:"Adam"
    ~nom_formation:"Université Côte d'Azur";
    renonce session
    ~nom_candidat:"Leo"
    ~nom_formation:"Université de Toulon";
    renonce session
    ~nom_candidat:"Raphael"
    ~nom_formation:"Université de Lyon";
    renonce session
    ~nom_candidat:"Louise"
    ~nom_formation:"Université de Paris";
  renonce session
    ~nom_candidat:"Alice"
    ~nom_formation:"Université de Grenoble";

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";
  affiche_voeux_en_attente session "Raphael";
  affiche_propositions_en_attente session "Raphael";
  affiche_voeux_en_attente session "Louise";
  affiche_propositions_en_attente session "Louise";
  affiche_voeux_en_attente session "Alice";
  affiche_propositions_en_attente session "Alice";
  affiche_voeux_en_attente session "Emma";
  affiche_propositions_en_attente session "Emma";

  renonce session
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Paris";
  renonce session
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Lyon";
  renonce session
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Toulon";
  renonce session
    ~nom_candidat:"Emma"
    ~nom_formation:"Université de Grenoble";

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";
  affiche_voeux_en_attente session "Raphael";
  affiche_propositions_en_attente session "Raphael";
  affiche_voeux_en_attente session "Louise";
  affiche_propositions_en_attente session "Louise";
  affiche_voeux_en_attente session "Alice";
  affiche_propositions_en_attente session "Alice";
  affiche_voeux_en_attente session "Emma";
  affiche_propositions_en_attente session "Emma";