open Definitions

let algo ?(affiche_config=false) entree =
  if (entree_valide entree) then
    begin
      let n = entree.n in
      let k = ref 0 in
      let xH = ref 0 in
      let xF = ref 0 in
      let conf = {rang_appel_de = Array.make n 0; fiance_de = Array.make n None} in
      let prefTab = Array.make n [] in
      let celibTab = Array.make n 1 in
      let ret = ref ([] : sortie) in

      while (!k < n) do
        for i = 0 to (n - 1) do
          if (celibTab.(i) = 1) then 
            begin
              xF := entree.liste_appel_de.(i).(conf.rang_appel_de.(i));
              prefTab.(!xF) <- i :: prefTab.(!xF);
            end;
        done;
        
        for i = 0 to (n - 1) do
          while (prefTab.(i) <> []) do
            xH := (match prefTab.(i) with | h :: _ -> h | _ -> -1);
            prefTab.(i) <- (match prefTab.(i) with | _ :: t -> t | _ -> []);

            let actFiance = (match conf.fiance_de.(i) with | None -> -1 | Some v -> v) in
            if (actFiance = -1) then
              begin
                k := !k + 1;
                conf.fiance_de.(i) <- Some !xH;
                celibTab.(!xH) <- 0;
              end
            else if (entree.prefere.(i) !xH actFiance) then
              begin
                conf.fiance_de.(i) <- Some !xH;
                celibTab.(!xH) <- 0;
                celibTab.(actFiance) <- 1;
                conf.rang_appel_de.(actFiance) <- conf.rang_appel_de.(actFiance) + 1;
              end
            else
              begin
                conf.rang_appel_de.(!xH) <- conf.rang_appel_de.(!xH) + 1;
              end;
          done;
        done;
        
        if (affiche_config) then
            print_configuration conf;
      done;

      for i = 0 to (n - 1) do
        let fiance = (match conf.fiance_de.(i) with | Some v -> v | _ -> -1) in
        ret := (fiance, i) :: !ret;
      done;
      !ret;
    end
  else raise (Invalid_argument "")