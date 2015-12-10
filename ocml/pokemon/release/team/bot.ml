open Team
open Definitions
open Constants
open Util

let name = "bot" 

let _ = Random.self_init ()

(* OUR CONSTANTS *)
let cDEFAULT_INV = [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
                        cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED]

let cINITIAL_TEAM = ([], cDEFAULT_INV, cSTEAMMON_CREDITS)


(******************************************** STARTER REQUEST FUNCTIONS ***************************************)

(*Returns a list of the live Steammons out of steammon list mons*)
let get_live_mons (mons:steammon list) =
  List.fold_left (fun acc m -> if (m.curr_hp > 0) then m::acc else acc) [] mons

(*Returns a list of just the damaging moves that have pp left from move list moves*)
let get_damaging_moves (moves:move list) =
  List.fold_left (fun acc move -> if ((move.power > 0) && (move.pp_remaining > 0)) then move::acc else acc) [] moves

(*Returns a list of a 3-tuple containing the steammon's name, the effectiveness
  of its most effective move against the opponent, and the effectiveness of the
  opponent's most effective move against it.

val steammon list -> steammon -> (string * effectiveness * effectiveness) list*)
let make_strength_resistance_list (my_mons:steammon list) (opponent:steammon) =
  (*Picks most effective effectiveness of e1 and e2*)
  let take_most_effective (e1:effectiveness) (e2:effectiveness) =
    match (e1, e2) with
    | (Ineffective, _) -> e2
    | (_, Ineffective) -> e1
    | (NotVeryEffective, _) -> e2
    | (_, NotVeryEffective) -> e1
    | (Regular, _) -> e2
    | (_, Regular) -> e1
    | (SuperEffective, _) -> e1 in
  (*Gets the highest effectiveness out of the damaging moves in moves against mon*)
  let movepool_effectiveness (moves:move list) (mon:steammon) =
    let mon_types = (mon.first_type, mon.second_type) in
    List.fold_left (fun acc (m:move) ->
      take_most_effective acc (fst (calculate_type_matchup m.element mon_types)))
      Ineffective moves in
  let helper acc (mon:steammon) =
    let my_moves = get_damaging_moves [mon.first_move;mon.second_move;mon.third_move;mon.fourth_move] in
    let opponent_moves = get_damaging_moves [opponent.first_move;opponent.second_move;opponent.third_move;opponent.fourth_move] in
    (mon, movepool_effectiveness my_moves opponent, movepool_effectiveness opponent_moves mon)::acc in
  List.fold_left helper [] my_mons

(*Picks the next Steammon to be played by looking at the opponent's current
Steammon. Picks steammon with best combination of effective moves and resistance
to the opponent's moves. Returns its string representation*)
let pick_next_mon (my_mons:steammon list) (opponent:steammon) : string = 
  (*Assigns integer value to given steammon strength and resistance tuple.
  Higher values mean better performing steammon*)
  let assign_value (mon, str, weak) =
    match (str, weak) with
    | (Ineffective, _) -> 0.
    | (_, Ineffective) -> 8.
    | (NotVeryEffective, NotVeryEffective) -> 2.
    | (NotVeryEffective, _) -> 1.
    | (_, NotVeryEffective) -> 6.
    | (Regular, Regular) -> 5.
    | (Regular, SuperEffective) -> 3.
    | (SuperEffective, SuperEffective) -> 4.
    | (SuperEffective, Regular) -> 7. in
  (*Returns the best steammon to use given its current hp as well as
  strength and resistance against the given opponent*)
  let pick_best l : steammon option =
    fst (List.fold_left (fun acc x -> let v = (assign_value x) in 
      let (mon, str, weak) = x in
      let hp_percent = ((float_of_int mon.curr_hp)/.(float_of_int mon.max_hp)) in
      if ((v *. hp_percent) >= (snd acc)) 
      then (Some mon, v)
      else acc) (None, 0.) l) in
  let live_mons = get_live_mons my_mons in
  let str_res_list = make_strength_resistance_list live_mons opponent in
  match (pick_best str_res_list) with
  | None -> failwith "pick_best probably failed"
  | Some m -> m.species

(*Function to pick the first Steammon to be played. Looks at all the opponent's
steammon and picks the Steammon in its own team with the best average effectiveness
move pool*)
let pick_first_mon (my_mons:steammon list) (opp_mons:steammon list) =
  let rec increment (l:(string * int) list) (discard:(string * int) list) (e:string) : (string * int) list =
    match l with
    | [] -> (e, 1)::discard
    | (x, i)::xs -> if (e = x) then ((x, i+1)::xs)@discard
                    else increment xs ((x, i)::discard) e in
  let best_mon_list = List.map (pick_next_mon my_mons) opp_mons in
  let best_count_list = List.fold_left (fun acc smn -> increment acc [] smn) [] best_mon_list in
  fst (List.fold_left (fun acc pair -> if ((snd pair) >= (snd acc)) then pair else acc) ("Fail", 0) best_count_list)

  (*Handles a StarterRequest, both before the start of the battle and if a steammon has fainted*)
let handle_starter_request (my_mons:steammon list) (opp_mons:steammon list) =
  let first_pick () =
    ((List.fold_left (fun acc m -> if (m.curr_hp = 0) then false else acc) true my_mons) &&
     (List.fold_left (fun acc m -> if (m.curr_hp = 0) then false else acc) true opp_mons))in
  if (first_pick ()) then pick_first_mon my_mons opp_mons
  else pick_next_mon my_mons (List.hd opp_mons)

(**************************************** ACTION REQUEST FUNCTIONS ************************************************************)

(* return STAB multiplier for a mon using a mov *)
let stab_of_mon (mon:steammon) (mov: move) : float =
  if ((mon.first_type = Some (mov.element)) ||
        (mon.second_type = Some (mov.element))) then
    cSTAB_BONUS
  else
    1.0

(* return burn_weakness for a mon *)
let burn_weakness_of_mon (mon:steammon): float =
  if (mon.status = Some(Burned)) then
    cBURN_WEAKNESS
  else
    1.0
    
(* Use calculate_type_matchup to determine the effectiveness and
 * multiplier of a move on a steammon
 * calculate_type_matchup is called from util.ml*)
let eff_and_multiplier (mon: steammon) (mov: move) =
  let atk_type = mov.element in
  let mon_types = (mon.first_type, mon.second_type) in
  calculate_type_matchup atk_type mon_types

(* Determine damage done by move mov by active_mon on opponent_mon
 * st_multiplier is the steamtype multiplier.
 * calculate_damage is called from util.ml*)
let det_damage (active_mon: steammon) (mov: move) (opponent_mon: steammon) : int =
  let active_mon_atk = calc_attackers_attack active_mon mov in
  let opponent_mon_def = calc_defenders_defense opponent_mon mov in
  let stab = stab_of_mon active_mon mov in
  let burn_weakness = burn_weakness_of_mon active_mon in
  let (effectvnss, st_multiplier) = eff_and_multiplier opponent_mon mov in
  let rand = (float_of_int (Random.int 100))/. 100.0 in
  let multiplier = stab*.st_multiplier*.burn_weakness*.rand in
  calculate_damage active_mon_atk opponent_mon_def mov.power multiplier

(* Gets a list of the inputed steammon's moves with pp > 0*)
let get_steam_moves (st:steammon) =
  let mv_lst = [st.first_move;st.second_move;st.third_move;st.fourth_move] in
  List.fold_left (fun acc ele -> if (ele.pp_remaining> 0) then ele::acc else acc) [] mv_lst 

(* sorts your steammon's list of moves based on the damage to move inflicts
   to the opponent's steammon. There moves are returned as tuples of (damage,move)
   with the head of the list being the move that deals the most damage *)
let mon_moves_w_dmg (opp_mon: steammon) (your_mon: steammon) = 
  let det_dmg (mv: move)=
    if mv.accuracy> 85 then 
      (det_damage your_mon mv opp_mon, mv) 
    else (((15 + mv.accuracy)*(det_damage your_mon mv opp_mon))/100, mv)  
  in 
  let move_w_dmg = List.map (det_dmg) (get_steam_moves your_mon) in 
  let sort_dmg = ((List.sort (compare) ((List.map (fst) move_w_dmg)))) in
  List.fold_left (fun acc ele -> (ele, List.assoc ele move_w_dmg)::acc) [] sort_dmg 


(* determines the action a steammon should take. If non of the steammon's moves don't do damage
   over a threshold then the steammon is switched out for one which will fight the curent steammon 
   most effectivly. If the steammon has no pp left on any of his moves, it is also switched out*)

let det_move (mvs: (int*move) list) (opp_mons: steammon list) (your_mons: steammon list) =
  match mvs with 
  | (dmg, mv)::tl -> if ((List.hd opp_mons).curr_hp - dmg <= 0) || ( ((float_of_int dmg) /. (float_of_int  ( (List.hd opp_mons).max_hp))) >= 0.2) 
                then (print_endline (mv.name);
                UseMove(mv.name) )
              else SwitchSteammon (pick_first_mon (List.tl your_mons) opp_mons)
  | _ -> SwitchSteammon (pick_first_mon (List.tl your_mons) opp_mons)
 

(* determines the action the active steammon should take based on the opponent's
   active steammon *)
let do_action (opp_mons: steammon list) (your_mons: steammon list) =
  let mon_dmg = mon_moves_w_dmg (List.hd opp_mons) (List.hd your_mons) in
  det_move mon_dmg opp_mons your_mons

(********************************* PICK REQUEST FUNCTIONS **********************************************)

(*Helper for handle_pick_request. Does actual selection of Steammon with
the highest attacks and then compares them to the opponent's current team
for coverage of effectiveness*)
let draft_best_mon (sp:steam_pool) (opp_mon:steammon list) =
  (*Sorts given steam_pool in decreasing order of attack*)
  let sort_by_attack (sp:steam_pool) =
    List.sort (fun m1 m2 -> let m1_best = max m1.attack m1.spl_attack in
      let m2_best = max m2.attack m2.spl_attack in
      m2_best - m1_best) sp in
  (*Returns a list of the top num attackers in sp*)
  let rec get_top_attackers (sp:steam_pool) (num:int) (acc:steammon list) =
    match (num, sp) with
    | (0, _) -> acc
    | (_, x::xs) -> get_top_attackers xs (num-1) (x::acc)
    | _ -> failwith "Not enough Steammon in filtered pool in get_top_attackers" in
  let sorted_pool = sort_by_attack sp in
  match opp_mon with
  | [] -> (List.hd sorted_pool).species
  | x::xs -> let how_many = (let tentative = (List.length sp)/5 in
      if (tentative = 0) then 1
      else tentative) in
      let best = get_top_attackers sorted_pool how_many [] in
      pick_first_mon best opp_mon

(*Handles a PickRequest by picking an affordable Steammon from sp with a compromise for having
the highest attack or special attack and type effectiveness agains the opponent's current team.
If no Steammon is affordable, an arbitrary Steammon is selected*)
let handle_pick_request (c:color) (gs:game_status_data) (sp:steam_pool) =
  (*Folds over given steam_pool and returns a list that only includes the Steammon
  whose cost is less than or equal to cash*)
  let filter_by_cost (sp:steam_pool) (cash:int) =
    List.fold_left (fun acc mon -> if (mon.cost <= cash) then mon::acc else acc) [] sp in
  let (r, b) = gs in
  let (my_team, opp_team) = if c = Red then (r, b) else (b, r) in
  let (my_mon, _, my_cash) = my_team in
  let (opp_mon, _, _) = opp_team in
  let affordable_mon = filter_by_cost sp my_cash in
  match affordable_mon with
  | [] -> (List.hd sp).species
  | x::xs -> draft_best_mon affordable_mon opp_mon

(**************************** PICK INVENTORY REQUEST FUNCTIONS *******************************)

(* Helper function for PickInventoryRequest
let cINITIAL_CASH = 3000
let cCOST_ETHER = 200
let cCOST_MAXPOTION = cCOST_MAXPOTION
let cCOST_FULLHEAL = cCOST_FULLHEAL
let cCOST_REVIVE = cCOST_REVIVE
let cCOST_XATTACK = 400
let cCOST_XDEFEND = 300
let cCOST_XSPEED = 300

let cITEM_COSTS = [cCOST_ETHER;cCOST_MAXPOTION;cCOST_REVIVE;cCOST_FULLHEAL;
                        cCOST_XATTACK;cCOST_XDEFEND; cCOST_XSPEED]

   | PickInventoryRequest (gr) -> print_string "bot is handling PickInventoryRequest"; PickInventory(
          [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
           cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])  *)



                 
let cITEM_COSTS = [(cCOST_ETHER,0);(cCOST_MAXPOTION,1);(cCOST_REVIVE,2);(cCOST_FULLHEAL,3);
                        (cCOST_XATTACK,4);(cCOST_XDEFEND,5); (cCOST_XSPEED,6)]

let elim_price (cost:int) =
  List.filter (fun ele -> fst ele <= cost) cITEM_COSTS

let min_item () = 
  let item_lst = List.map fst cITEM_COSTS in
  List.hd (List.rev (List.sort compare item_lst))

let buy_priorities (_:unit) = 
  let inv = (Array.make 7 0) in
  let cash = ref cINITIAL_CASH in
  let counter = ref 0 in
  while (!cash > min_item () ) do
    match !counter with 
    | 0 -> if !cash>= cCOST_MAXPOTION then 
             ((inv.(1) <- (inv.(1) +1)); (counter := !counter + 1); (cash := (!cash - cCOST_MAXPOTION)); )
           else (counter := !counter + 1); 
    | 1 -> if !cash>= cCOST_FULLHEAL then 
             ((inv.(2) <- (inv.(2) +1)); (counter := !counter + 1); (cash := (!cash - cCOST_FULLHEAL)); )
           else (counter := !counter + 1);
    | 2 ->if !cash>= cCOST_REVIVE then 
             ((inv.(3) <- (inv.(3) +1)); (counter := !counter + 1); (cash := (!cash - cCOST_REVIVE)); )
           else  (counter := !counter + 1);
    | _ ->  let (price, itm) =  List.hd (elim_price (min_item ())) in 
              (cash:= (!cash- price)); (inv.(itm) <- (inv.(itm) +1));
    done ;
    Array.to_list inv  








(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> print_string "bot is handling teamNameRequest\n"; SendTeamName(name)
    | StarterRequest(gs)->
        print_string "bot is handling StarterRequest\n";
        let (a1,b1) = gs in
        let (me, opp) = if c = Red then (a1, b1) else (b1, a1) in
        let (my_mons, my_pack, my_credits) = me in
        let (opp_mons, opp_pack, opp_credits) = me in
        SelectStarter(handle_starter_request my_mons opp_mons)
    | PickRequest(c, gs, _, sp) ->
        print_string "bot is handling PickRequest\n";
        (match sp with
         | h::t ->
            let my_pick = handle_pick_request c gs sp in
            PickSteammon (my_pick)
         | [] -> failwith "no steammon to pick!") 
    | ActionRequest (gr) ->
        print_string "bot is handling ActionRequest\n";
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        let opp_team = if c = Red then b1 else a1 in
        let (opp_mons, opp_pack, opp_credits) = opp_team in
        do_action opp_mon mons
        
	 | PickInventoryRequest (gr) -> print_string "bot is handling PickInventoryRequest"; PickInventory(buy_priorities ())

   
let () = run_bot handle_request
