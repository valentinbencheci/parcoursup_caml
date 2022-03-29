(* ###################### - DESCRIPTION TEST - ###################### *)
(* On a 2 candidats qui veulent la meme formation, mais celle-la 
apartient au 3eme candidat. Il est interessant de voir comment 
l'algo va fonctionner dans cette situation*)
(* ################################################################## *)

open Test_parcoursup_utils

let session = nouvelle_session ()

let () =
  ajoute_candidat session ~nom_candidat:"Adam";
  ajoute_candidat session ~nom_candidat:"Leo";
  ajoute_candidat session ~nom_candidat:"Raphael";

  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;

  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Adam"
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Leo"
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session
    ~rang_repondeur:(Some 1)
    ~nom_candidat:"Raphael"
    ~nom_formation:"Université Côte d'Azur";
  ajoute_voeu session
    ~rang_repondeur:(Some 2)
    ~nom_candidat:"Leo"
    ~nom_formation:"Université de Toulon";
  ajoute_voeu session
    ~rang_repondeur:(Some 2)
    ~nom_candidat:"Raphael"
    ~nom_formation:"Université de Toulon";

  ajoute_commission session
    ~nom_formation:"Université Côte d'Azur"
    ["Adam";"Leo";"Raphael"];
  ajoute_commission session
    ~nom_formation:"Université de Toulon"
    ["Adam";"Leo";"Raphael"];

  reunit_commissions session;

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";
  affiche_voeux_en_attente session "Raphael";
  affiche_propositions_en_attente session "Raphael";

  renonce session
    ~nom_candidat:"Adam"
    ~nom_formation:"Université Côte d'Azur";

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";
  affiche_voeux_en_attente session "Raphael";
  affiche_propositions_en_attente session "Raphael";