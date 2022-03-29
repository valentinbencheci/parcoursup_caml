(* ###################### - DESCRIPTION TEST - ###################### *)
(* On ajoute 2 fois le meme candidat et on regarde si on n'aura pas 
des dublicates. En plus on ajoute une formation pour laquelle il n'y a 
pas des candidatures. *)
(* ################################################################## *)

open Test_parcoursup_utils

let session = nouvelle_session ()

let () =
  ajoute_candidat session ~nom_candidat:"Adam";
  ajoute_candidat session ~nom_candidat:"Leo";
  ajoute_candidat session ~nom_candidat:"Adam";

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

  ajoute_commission session
    ~nom_formation:"Université Côte d'Azur"
    ["Adam";"Leo"];
  ajoute_commission session
    ~nom_formation:"Université de Toulon"
    ["Adam";"Leo"];

  reunit_commissions session;

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";

  renonce session
    ~nom_candidat:"Adam"
    ~nom_formation:"Université Côte d'Azur";

  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Leo";
  affiche_propositions_en_attente session "Leo";