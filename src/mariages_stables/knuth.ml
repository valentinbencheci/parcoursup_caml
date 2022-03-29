open Definitions

let algo ?(affiche_config=false) entree =
  if (entree_valide entree) then
    begin
      let n = entree.n in
      let k = ref 0 in
      let xH = ref 0 in
      let xF = ref 0 in
      let conf = {rang_appel_de = Array.make n 0; fiance_de = Array.make n None} in
      let ret = ref ([] : sortie) in

      while (!k < n) do
          xH := !k;
          
          while (!xH <> -1) do
            xF := entree.liste_appel_de.(!xH).(conf.rang_appel_de.(!xH));

            let actFiance = (match conf.fiance_de.(!xF) with | None -> -1 | Some v -> v) in
            if (actFiance = -1) then
              begin
                conf.fiance_de.(!xF) <- Some !xH;
                xH := -1;
              end
            else if (entree.prefere.(!xF) !xH actFiance) then
              begin
                conf.fiance_de.(!xF) <- Some !xH;
                xH := actFiance;
                conf.rang_appel_de.(!xH) <- conf.rang_appel_de.(!xH) + 1;
              end
            else
              begin
                conf.rang_appel_de.(!xH) <- conf.rang_appel_de.(!xH) + 1;
              end;
          
          if (affiche_config) then
            print_configuration conf;
          done;

          k := !k + 1;
      done;

      for i = 0 to (n - 1) do
        let fiance = (match conf.fiance_de.(i) with | Some v -> v | _ -> -1) in
        ret := (fiance, i) :: !ret;
      done;
      !ret;
    end
  else raise (Invalid_argument "")