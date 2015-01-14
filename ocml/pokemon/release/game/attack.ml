open Definitions
open Util
open Constants
open Netgraphics
open Initialization
open State


(* Get the lead steammon for use_move *)
let get_lead_steammon (gs: game_status_data) (c:color) =
  let (rteam, bteam) = gs in
  match c with
  | Red -> let (rmons, _, _) = rteam in (List.hd rmons)
  | Blue -> let (bmons, _, _) = bteam in (List.hd bmons)

(* Determine the move actually used by the steammon, i.e. check if
 * it has sufficient pp and return the Struggle option if not enough pp
 * is available.*)
let actual_move (movop: move option): move option =
  match movop with
  | None -> None
  | Some mov -> if mov.pp_remaining = 0 then
                  Some (Table.find move_table "Struggle")
                else Some mov

(* Function to use a move represented by string move from the active Steammon of the team of
 * color c. Takes in the current game state and returns an updated game
 * state with both team_data updated based on the move c *)
let use_move (gs: game_status_data) (movstr: string) (c:color): game_status_data =
  let active_mon = get_lead_steammon gs c in
  let active_monstr = active_mon.species in
  let movop = get_move_from_steammon active_mon movstr in
  let actual_mov = actual_move movop in
  let rand = Random.int 100 in
  match active_mon.status with
  | Some (Frozen) ->
     print_string active_monstr; print_endline "is frozen and cannot attack!"; gs
  | Some (Asleep) ->
     print_string active_monstr; print_endline "is asleep and cannot attack!"; gs
  | Some (Confused) ->
     print_string active_monstr; print_endline "is confused!";
     if (rand < cSELF_ATTACK_CHANCE) then
       failwith "to handle attack_self"
     else
       failwith "to handle normal attack"
  | Some (Paralyzed) ->
     print_string active_monstr; print_endline "is paralyzed!";
     if (rand < cPARALYSIS_CHANCE) then
       begin
       print_string active_monstr; print_endline "can't move!"; gs
       end
     else
       failwith "todo" (*normal attack*)
  | Some (Poisoned)
  | Some (Burned)
  | None ->  failwith "todo"

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
 * multiplier of a move on a steammon *)
let eff_and_multiplier (mon: steammon) (mov: move) =
  let atk_type = mov.element in
  let mon_types = (mon.first_type, mon.second_type) in
  calculate_type_matchup atk_type mon_types

(* Determine damage done by move mov by active_mon on opponent_mon
 * st_multiplier is the steamtype multiplier. *)
let det_damage (active_mon: steammon) (mov: move) (opponent_mon: steammon) : int =
  let active_mon_atk = calc_attackers_attack active_mon mov in
  let opponent_mon_def = calc_defenders_defense opponent_mon mov in
  let stab = stab_of_mon active_mon mov in
  let burn_weakness = burn_weakness_of_mon active_mon in
  let (effectvnss, st_multiplier) = eff_and_multiplier opponent_mon mov in
  let rand = (float_of_int (Random.int 100))/. 100.0 in
  let multiplier = stab*.st_multiplier*.burn_weakness*.rand in
  calculate_damage active_mon_atk opponent_mon_def mov.power multiplier

(* handles a move that occurs, movop is the move option
 * returned by get_move_from_steammon. mov must have pp remaining as
 * checked by actual_move. c is the team color that has the move*)
let handle_attack (gs:game_status_data) (movop: move option) (c:color) =
  let active_mon = get_lead_steammon gs c in
  let active_monstr = active_mon.species in
  let opponent_mon = get_lead_steammon gs (invert_color c) in
  let opponent_monstr = opponent_mon.species in
  let rand = Random.int 100 in
  match movop with
  | None -> print_endline "Invalid move selected."; gs
  | Some mov -> if (rand < mov.accuracy || mov.target = User) then
                  let movnew = {mov with pp_remaining = mov.pp_remaining - 1} in
                  let dmg = det_damage active_mon mov opponent_mon
                  in failwith "todo"
                else
                  begin
                  print_string mov.name;
                  print_endline " missed!";
                  gs
                  end

(* Deal dmg worth of damage to a steammon, return the steammon after
 * damage has been applied. *)
let deal_dmg (mon:steammon) (dmg:int): steammon =
  if mon.curr_hp > dmg then
    {mon with curr_hp = mon.curr_hp-dmg}
  else
    {mon with curr_hp = 0}
