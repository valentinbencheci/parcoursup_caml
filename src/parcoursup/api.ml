(********************************* UTILITAIRE DES ERREURS *********************************)
let erreurs = [|
(*0*)  "il y a deja un candidat avec ce nom";
(*1*)  "la capacite ne peut pas etre negative";
(*2*)  "il y a deja une formation avec ce nom";
(*3*)  "il n'existe pas une formation/candidat avec ce nom";
(*4*)  "il y a deja un commission avec ce nom";
(*5*)  "le rang de voeu ne peut pas etre negatif";
(*6*)  "pas tous les commissions se sont reunis";
  |]

let print_erreur ?(printFlag = false) indice = 
  if (printFlag) then
    print_string ("ERREUR : " ^ (string_of_int indice) ^ " " ^ erreurs.(indice))

(********************************* TYPES DES DONNEES *********************************)
type formation = {
  mutable nom : string;
  mutable capacite : int;
  mutable candidats : string list;
  mutable arretProposition : int;
}

type voeu = {
  mutable rang : int option;
  mutable formation : string;
}

type candidat = {
  mutable nom : string;
  mutable voeux : voeu list;
  mutable liste_propositions : string list;
  mutable liste_attente : (string * int) list;
}

type commission = {
  mutable nom_formation : string;
  mutable fonc_comparaison : (candidat1:string -> candidat2: string -> bool); 
}

type session = {
  mutable formations : formation list;
  mutable commissions : commission list;
  mutable candidats : candidat list;
}


(********************************* FONCTIONS UTILITAIRES *********************************)
let rec indice_candidat ?(index = 0) (liste : candidat list) nom = 
  match liste with
    | [] -> -1
    | h :: _ when (h.nom = nom) -> index
    | _ :: t -> indice_candidat t nom ~index:(index + 1)

let rec indice_formation ?(index = 0) (liste : formation list) nom = 
  match liste with
    | [] -> -1;
    | h :: _ when (h.nom = nom) -> index
    | _ :: t -> indice_formation t nom ~index:(index + 1)

let rec indice_voeu ?(index = 0) (liste : voeu list) formation = 
  match liste with
    | [] -> -1
    | h :: _ when (h.formation = formation) -> index
    | _ :: t -> indice_voeu t formation ~index:(index + 1)

let rec indice_commission ?(index = 0) (liste : commission list) nom_formation = 
  match liste with
    | [] -> -1
    | h :: _ when (h.nom_formation = nom_formation) -> index
    | _ :: t -> indice_commission t nom_formation ~index:(index + 1)

let rec update_candidats (liste : candidat list) nouveauCandidat = 
  match liste with 
    | [] -> []
    | h :: t when (h.nom = nouveauCandidat.nom) -> nouveauCandidat :: t
    | h :: t -> h :: (update_candidats t nouveauCandidat)

let rec update_formations (liste : formation list) (nouvelleFormation : formation) = 
  match liste with
    | [] ->[]
    | h :: t when (h.nom = nouvelleFormation.nom) -> nouvelleFormation :: t
    | h :: t -> h :: (update_formations t nouvelleFormation)

let rec update_voeux (liste : voeu list) nom_formation rang_repondeur = 
  match liste with
    | [] ->[]
    | h :: t when (h.formation = nom_formation) -> {rang = rang_repondeur; formation = h.formation} :: t
    | h :: t -> h :: (update_voeux t nom_formation rang_repondeur)

let rec trie_Candidats liste el func = 
  match liste with
    | [] -> [el]
    | [h] when (func ~candidat1:el ~candidat2:h) -> el :: [h]
    | [h] -> [h] @ [el]
    | h :: t when (func ~candidat1:el ~candidat2:h) -> el :: (h :: t)
    | h :: t -> h :: (trie_Candidats t el func)

let rec progressiveAppel liste func= 
  match liste with
    | [] -> []
    | h :: t -> trie_Candidats (progressiveAppel t func) h func

let rec indice_liste_attente ?(index = 0) liste el = 
  match liste with
    | [] -> -1
    | h :: t ->
      begin
        let (e0, _) = h in
        if (e0 = el) then
        index
        else
          indice_liste_attente t el ~index:(index + 1)
      end

let proposerFormation session =
  let ind_formation = ref 0 in
  let maxNbrFormation =  List.length session.formations in
  
  while (!ind_formation < maxNbrFormation) do
    let formation = (List.nth session.formations !ind_formation) in

    let i = ref (formation.arretProposition) in

    while ((!i < formation.capacite) && (List.length formation.candidats > formation.arretProposition)) do
      let tmpIndCandidat = indice_candidat session.candidats (List.nth formation.candidats !i) in
      let tmpCandidat = List.nth session.candidats tmpIndCandidat in
      tmpCandidat.liste_propositions <- tmpCandidat.liste_propositions @ [formation.nom];
      session.candidats <- update_candidats session.candidats tmpCandidat;
      formation.arretProposition <- formation.arretProposition + 1;
      i := !i + 1;
    done;
    session.formations <- update_formations session.formations formation;
    
    ind_formation := !ind_formation + 1;
  done

let max a b = 
  if (a >= b) then b else a

let rec maxRang (liste : int list) = 
  match liste with
    | [] -> -2
    | [h] -> h
    | h :: t -> max h (maxRang t)

let rec chercherRangVoeu (liste : voeu list) nom_formation = 
  match liste with 
    | [] -> Some (-1)
    | h :: _ when (h.formation = nom_formation) -> h.rang
    | _ :: t -> chercherRangVoeu t nom_formation

let generateRangList session ind_candidat =
  let i = ref 0 in
  let res = ref [] in
  let candidat = List.nth session.candidats ind_candidat in

  while (!i < (List.length candidat.liste_propositions)) do
    let proposition = List.nth candidat.liste_propositions !i in
    let rang = chercherRangVoeu candidat.voeux proposition in

    if (rang != None) then
      res := (match rang with | None -> -1 | Some v -> v) :: !res;
    i := !i + 1;
  done;
  !res

let analyserFormation session = 
  let ind_candidat = ref 0 in
  let maxCandidats = List.length session.candidats in

  while (!ind_candidat < maxCandidats) do
    let nouvelleProposListe = ref ([] : string list) in
    let candidat = List.nth session.candidats !ind_candidat in
    let i = ref 0 in
    let nbrPropositions = List.length candidat.liste_propositions in

    while ((nbrPropositions > 0) && (!i < nbrPropositions)) do
      let maxRang = maxRang (generateRangList session !ind_candidat) in
      let nom_formation = List.nth candidat.liste_propositions !i in
      let rangProposition = chercherRangVoeu candidat.voeux nom_formation in

      if ((rangProposition = None) || (rangProposition = (Some maxRang))) then
        nouvelleProposListe := nom_formation :: !nouvelleProposListe
      else
        begin
          let ind_formation = indice_formation session.formations nom_formation in
          let nouvelleFormation = List.nth session.formations ind_formation in
          nouvelleFormation.capacite <- nouvelleFormation.capacite + 1;
          session.formations <- update_formations session.formations nouvelleFormation;
        end;
      
      i := !i + 1;
    done;
    candidat.liste_propositions <- !nouvelleProposListe;
    session.candidats <- update_candidats session.candidats candidat;
    ind_candidat := !ind_candidat + 1;
  done

let rec indice_string_list ?(index = 0) liste el = 
  match liste with
    | [] -> -1
    | h :: _ when (h = el) -> index
    | _ :: t -> indice_string_list t el ~index:(index + 1)

let generateListeAttente session = 
  let ind_candidat = ref 0 in
  let maxCandidats = List.length session.candidats in

  while (!ind_candidat < maxCandidats) do
    let nouvelleListeAttente = ref ([] : (string * int) list) in
    let candidat = List.nth session.candidats !ind_candidat in
    let i = ref 0 in
    let maxVoeux = List.length candidat.voeux in 

    while (!i < maxVoeux) do
      let actualVoeu = List.nth candidat.voeux !i in

      if ((indice_string_list candidat.liste_propositions actualVoeu.formation) = -1) then
        begin
          let ind_formation = indice_formation session.formations actualVoeu.formation in
          let actualFormation = List.nth session.formations ind_formation in
          let arretProposition = actualFormation.arretProposition in
          let positionCandidatInQueue = indice_string_list actualFormation.candidats candidat.nom in
          if ((positionCandidatInQueue - arretProposition) >= 0) then
            nouvelleListeAttente := (actualVoeu.formation, (positionCandidatInQueue - arretProposition)) :: !nouvelleListeAttente;
        end;
      i := !i + 1;
    done;
  
    candidat.liste_attente <- !nouvelleListeAttente;
    session.candidats <- update_candidats session.candidats candidat;
    ind_candidat := !ind_candidat + 1;
  done

let rec supprime_voeu liste nom_formation = 
  match liste with
    | [] -> []
    | h :: t when (h.formation = nom_formation) -> t
    | h :: t -> h :: (supprime_voeu t nom_formation)

let rec supprime_propos liste nom_formation = 
  match liste with 
    | [] -> []
    | h :: t when (h = nom_formation) -> t
    | h :: t -> h :: (supprime_propos t nom_formation)
  
let rec supprime_attente liste nom_formation = 
  match liste with
    | [] -> []
    | h :: t -> 
      begin
        let (e0, _) = h in
        if (e0 = nom_formation) then 
          t 
        else 
          h :: (supprime_attente t nom_formation)
      end
(********************************* NOTION DE SESSION *********************************)
let nouvelle_session () = 
  {formations = []; commissions = []; candidats = []}

let ajoute_candidat session ~nom_candidat = 
  if ((indice_candidat session.candidats nom_candidat) <> -1) then
      print_erreur 0
  else 
      session.candidats <- {nom = nom_candidat; voeux = []; liste_propositions = []; liste_attente = []} :: session.candidats

let ajoute_formation session ~nom_formation ~capacite =
  if (capacite < 0) then
    print_erreur 1
  else
    if ((indice_formation session.formations nom_formation) <> -1) then
      print_erreur 2
    else
      session.formations <- {nom = nom_formation; capacite = capacite; candidats = []; arretProposition = 0} :: session.formations

let ajoute_voeu session ~rang_repondeur ~nom_candidat ~nom_formation = 
  let ind_candidat = indice_candidat session.candidats nom_candidat in
  let ind_formation = indice_formation session.formations nom_formation in
  let checkRang = (match rang_repondeur with | Some v when (v < 0) -> true | _ -> false) in

  if ((ind_candidat = -1) || (ind_formation = -1) || checkRang) then
    if (checkRang) then
      print_erreur 5
    else
      print_erreur 3
  else
    let candidat = List.nth session.candidats ind_candidat in
    let formation = List.nth session.formations ind_formation in
    let ind_voeu = indice_voeu candidat.voeux nom_formation in
    
    if (ind_voeu = -1) then
      begin
        candidat.voeux <- {rang = rang_repondeur; formation = nom_formation} :: candidat.voeux;
        formation.candidats <- nom_candidat :: formation.candidats;
        session.candidats <- update_candidats session.candidats candidat;
        session.formations <- update_formations session.formations formation;
      end
    else
      begin
        candidat.voeux <- update_voeux candidat.voeux nom_formation rang_repondeur;
        session.candidats <- update_candidats session.candidats candidat;
      end

let ajoute_commission session ~nom_formation ~fonction_comparaison =   
  let ind_formation = indice_formation session.formations nom_formation in

  if (ind_formation = -1) then
    print_erreur 3
  else
    begin
    let indice_commission = indice_commission session.commissions nom_formation in
    
    if (indice_commission <> -1) then
      print_erreur 4
    else
      session.commissions <- {nom_formation = nom_formation; fonc_comparaison = fonction_comparaison} :: session.commissions;
    end

let checkAllCommissions session = 
  List.length session.commissions = List.length session.formations

let reunit_commissions session =
  let rec parcoursFormation (liste : formation list) =
    match liste with
      | [] -> []
      | h :: t ->
        begin
          let nomFormation = h.nom in
          let ind_commission = indice_commission session.commissions nomFormation in
          let commissionFormation = List.nth session.commissions ind_commission in
          h.candidats <- progressiveAppel h.candidats commissionFormation.fonc_comparaison;
          h :: (parcoursFormation t);
        end in
  if (checkAllCommissions session) then
    session.formations <- parcoursFormation session.formations
  else 
    print_erreur 6

let nouveau_jour session =
  let verifierArretProposition = ref false in

  if (checkAllCommissions session) then
    begin
      while (not !verifierArretProposition) do
        proposerFormation session;
        analyserFormation session;
        generateListeAttente session;

        let flag = ref 0 in
        let ind_formation = ref 0 in
        let maxNbrFormation = List.length session.formations in

        while (!ind_formation < maxNbrFormation) do
          let actualFormation = List.nth session.formations !ind_formation in
          let capaciteFormation = actualFormation.capacite in
          let arretProposition = actualFormation.arretProposition in
          let nbrCandidats = List.length actualFormation.candidats in

          if ((arretProposition = capaciteFormation) || (nbrCandidats = arretProposition)) then
            flag := !flag + 1;

          ind_formation := !ind_formation + 1;

          if (!flag = maxNbrFormation) then
            verifierArretProposition := true;

        done;
      done
    end
  else
    print_erreur 6

let renonce session ~nom_candidat ~nom_formation = 
  let ind_candidat = indice_candidat session.candidats nom_candidat in

  if (ind_candidat <> -1) then
    let candidat = List.nth session.candidats ind_candidat in
  
  let ind_formation_candidat = indice_voeu candidat.voeux nom_formation in

  if ((ind_candidat <> -1) && (ind_formation_candidat <> -1)) then
    begin
      if (ind_formation_candidat <> -1) then
        candidat.voeux <- supprime_voeu candidat.voeux nom_formation;

      let ind_cand_propos = indice_string_list candidat.liste_propositions nom_formation in
      if (ind_cand_propos <> -1) then
      begin
        candidat.liste_propositions <- (supprime_propos candidat.liste_propositions nom_formation);
        session.candidats <- update_candidats session.candidats candidat;
      end;

      let ind_cand_attente = indice_liste_attente candidat.liste_attente nom_formation in
      if (ind_cand_attente <> -1) then
      begin
        candidat.liste_attente <- supprime_attente candidat.liste_attente nom_formation;
        session.candidats <- update_candidats session.candidats candidat;
      end;

      let ind_formation = indice_formation session.formations nom_formation in
      let formation = List.nth session.formations ind_formation in
      let arretProposition = formation.arretProposition in
      let posQueueCandidat = indice_string_list formation.candidats nom_candidat in

      if (posQueueCandidat < arretProposition) then 
        begin
          formation.capacite <- formation.capacite + 1;
          session.formations <- update_formations session.formations formation;
        end
      else
        begin
          formation.candidats <- supprime_propos formation.candidats nom_candidat;
          session.formations <- update_formations session.formations formation;
        end
    end
  else
    print_erreur 3

let consulte_propositions session ~nom_candidat =
  let ind_candidat = indice_candidat session.candidats nom_candidat in

  if (ind_candidat = -1) then
    begin
      print_erreur 3;
      [];
    end
  else
    (List.nth session.candidats ind_candidat).liste_propositions

let consulte_voeux_en_attente session ~nom_candidat = 
  let ind_candidat = indice_candidat session.candidats nom_candidat in

  if (ind_candidat = -1) then
    begin
      print_erreur 3;
      [];
    end
  else
    (List.nth session.candidats ind_candidat).liste_attente