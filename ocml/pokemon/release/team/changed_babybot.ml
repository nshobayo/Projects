open Team
open Definitions
open Constants

let name = "babybot" 

let _ = Random.self_init ()

(********************************************************************************
 * Drafting phase: picking steammons                                            *
 ********************************************************************************)


(* option 1: make a global list of fav mon, and check if those favorite mons are in the steammpool, if so, pick it
   eventually, we want to eliminate the weak ones *)

(* algorithm: ranks all steammon in steampool *)

(* since picking lead steammon would just return the head of the stammon list, choose favorite? *)




(********************************************************************************
 * Inventory phase: buy items                                                   *
 ********************************************************************************)
(* cash is 3000 
   don't want anything that changes modifiers (xAttack, xDefend, )
   ether not usualy useful MAXPOTION, FULLHEAL, REVIVE are pretty good *)





(********************************************************************************
 * BATTLE phase:                                                                *
 ********************************************************************************)

(* check what the opponent's active mon is.
   check opponent's leading mon, see if our mon has an attack that's super effective against that type,
   if yes, then do it
   if no, then we want to check if oppo mon's attack is super effective to ours, we might want to switch
   to a mon that would counteract 

 *)


(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> print_string "babybot is handling teamNameRequest\n"; SendTeamName(name)
    | StarterRequest(gs)->
        print_string "babybot is handling StarterRequest\n";
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        let pick = 
          try print_string "babybot finding mon with hp > 0"; List.find(fun x -> x.curr_hp > 0) mons 
          with _ -> (List.hd mons) in
          SelectStarter(pick.species)
    | PickRequest(_, _, _, sp) ->
        print_string "babybot is handling PickRequest\n";
        (match sp with
         | h::t ->
            let length = List.length sp in
            let my_pick = List.nth sp (Random.int length) in
              PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
        print_string "babybot is handling ActionRequest\n";
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        (match mons with
        | h::t ->
            if (h.first_move).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_move).name)) in
                UseMove((h.first_move).name)
            else if ((h.second_move).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_move).name)) in
                UseMove((h.second_move).name)
            else if ((h.third_move).pp_remaining >0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_move).name)) in
                UseMove((h.third_move).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_move).name)) in
                UseMove((h.fourth_move).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
	 | PickInventoryRequest (gr) -> print_string "babybot is handling PickInventoryRequest"; PickInventory(
					[cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				 cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])
let () = run_bot handle_request
