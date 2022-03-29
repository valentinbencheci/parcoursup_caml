open Definitions

module type PIOCHE = sig
  type 'a t
    val of_list: 'a list -> 'a t 
    val pioche : 'a t -> 'a option 
    val defausse : 'a -> 'a t -> unit 
end



module Pile : PIOCHE = struct
  
  type 'a t = {mutable data : 'a list}

  let of_list l = 
    {data = l}

  let pioche p = 
    match p.data with
      | [] -> None
      | h::b -> 
        begin
          p.data <- b; 
          Some h;
        end

  let defausse x p =
    p.data <- x :: p.data

end



module File : PIOCHE = struct
  
  type 'a t = {mutable data : 'a list}

  let of_list l = 
    {data = l}

  let pioche p =
    ignore p;
    match p.data with
      | [] -> None
      | h::b -> 
        begin
          p.data <- b; 
          Some h;
        end

  let defausse x p =
    p.data <- p.data @ [x]

end



module Algo(P:PIOCHE) = struct
  
  let run entree = 
    if (entree_valide entree) then
      begin
        let n = entree.n in
        let tmpX = ref [] in
        let res = ref ([]:sortie) in
        let conf = {rang_appel_de = Array.make n 0; fiance_de = Array.make n None} in

        for i = 0 to (n - 1) do
          tmpX := i :: !tmpX
        done;
        
        let piX = P.of_list !tmpX in
        let tmp_xH = ref (P.pioche piX) in
        
        while (!tmp_xH <> None) do
          begin
            let xH = (match !tmp_xH with | None -> -1 | Some v -> v) in
            let xF = entree.liste_appel_de.(xH).(conf.rang_appel_de.(xH)) in

            if (conf.fiance_de.(xF) = None) then
              begin
                conf.fiance_de.(xF) <- Some xH;
              end
            else 
            begin
              let actFiance = (match conf.fiance_de.(xF) with | None -> -1 | Some v -> v) in
              if (entree.prefere.(xF) xH actFiance) then
                begin
                  P.defausse actFiance piX;
                  conf.rang_appel_de.(actFiance) <- conf.rang_appel_de.(actFiance) + 1;
                  conf.fiance_de.(xF) <- Some xH;
                end
              else
                begin
                  P.defausse xH piX;
                  conf.rang_appel_de.(xH) <- conf.rang_appel_de.(xH) + 1; 
                end;
            end;

            tmp_xH := P.pioche piX;
          end
        done;

        for i = 0 to (n - 1) do
          res := !res @ [((i:homme), entree.liste_appel_de.(i).(conf.rang_appel_de.(i)))];
        done;
        !res;
      end
    else raise (Invalid_argument "")

end
