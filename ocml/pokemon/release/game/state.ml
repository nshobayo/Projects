open Definitions
open Util
open Constants
open Netgraphics
open Initialization


(** OUR CONSTANTS *)
let cDEFAULT_INV = [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
                        cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED]

let cITEM_COSTS = [cCOST_ETHER;cCOST_MAXPOTION;cCOST_REVIVE;cCOST_FULLHEAL;
                        cCOST_XATTACK;cCOST_XDEFEND; cCOST_XSPEED]

let cINITIAL_TEAM = ([], cDEFAULT_INV, cSTEAMMON_CREDITS)

(************************************************************************************)
(** Draft phase functions *)
(* Returns a steammon with the lowest cost in the steammon_pool.
 * Assumes that the mon_table has at least one steammon remaining.
 * helper function for pick_steammon *)
let lc_steammon (): steammon =
  let smlst = hash_to_list mon_table in
  List.fold_left (fun a b -> if a.cost < b.cost then a else b) (List.hd smlst) smlst

(* s is the string representing the steammon*)
let pick_steammon monstr team =
  let mon = Table.find mon_table monstr in
  let moncst = mon.cost in
  let (mons, inv, cre) = team in
  if cre > moncst then
    begin
    Table.remove mon_table monstr;
    print_endline ("picked a " ^  monstr);
    (mon::mons, inv, cre-moncst)
    end
  else
    begin
    let lcmon = lc_steammon () in
    let lcmoncst = lcmon.cost in
    Table.remove mon_table lcmon.species;
    print_endline ("picked a " ^ lcmon.species);
    if lcmoncst > cre then
      (lcmon::mons, inv, 0)
    else
      (lcmon::mons, inv, cre-lcmoncst)
    end

(* Return the number of steammons in a team *)
let num_steammons (team: team_data) =
  let (mons, _, _) = team in
  List.length mons

(************************************************************************************)
(** inventory phase functions *)
let inv_cost inv =
  List.fold_left2 (fun a item cost -> a + item*cost) 0 inv cITEM_COSTS

(* lets team pick an inventory invnew if the cost is ok *)
let pick_inventory invnew team =
  let (mons, inv, cre) = team in
  let total_cost = inv_cost (invnew) in
    if cINITIAL_CASH < total_cost then
      (mons, inv, cre)
    else
      (mons, invnew, cre)

(* Prints out a list of item quantities in order for debugging *)
let print_inv inv =
  List.fold_left (fun a x -> print_int x; print_char ' ';) () inv

(** Functions to help implement apply_effect on a steammon*)
(* Restores i pp to a move mov*)
let restore_pp_move i c mov =
  let pptmp = mov.pp_remaining + i in
  if pptmp > mov.max_pp then
    begin
    Netgraphics.add_update
       (AdditionalEffects
          [(RestoredPP (mov.max_pp - mov.pp_remaining), c)]);
    {mov with pp_remaining = mov.max_pp}
    end
  else
    begin
    Netgraphics.add_update(AdditionalEffects[(RestoredPP i, c)]);
    {mov with pp_remaining = pptmp}
    end

(* restore_pp i mon
 * restores i pp to all of mon's moves *)
let restore_pp i c mon =
  print_endline "PP was restored";
  {mon with
    first_move = restore_pp_move i c mon.first_move;
    second_move = restore_pp_move i c mon.second_move;
    third_move = restore_pp_move i c mon.third_move;
    fourth_move = restore_pp_move i c mon.fourth_move}

(* Checks if an int i is a valid modifier value,
 * i.e. it lies between -6 and 6 inclusive.*)
let is_valid_modifier (i:int): bool =
  if ((i < 7) && (i> (-7))) then true
  else false

(* modifies the stat s of a steammon by an integer i from color c *)
let mod_stat s i c mon =
  print_string (mon.species ^ "'s");
  match s with
  | Atk ->
     if (is_valid_modifier (mon.mods.attack_mod+i)) then
       begin
         Netgraphics.add_update(AdditionalEffects([StatModified(Atk, i), c]));
         print_string "attack changed "; print_int i;
         print_endline " stages";
         { mon with
           mods = {mon.mods with
                    attack_mod = (mon.mods.attack_mod+i)} }
       end
     else mon
  | Def ->
     if (is_valid_modifier (mon.mods.defense_mod+i)) then
       begin
         Netgraphics.add_update(AdditionalEffects([StatModified(Def, i), c]));
         print_string "defense changed ";
         print_int i;
         print_endline " stages";
         { mon with
           mods = {mon.mods with
                    defense_mod = (mon.mods.defense_mod+i)} }
           end
     else mon
  | SpA ->
     if (is_valid_modifier (mon.mods.spl_attack_mod+i)) then
       begin
         Netgraphics.add_update(AdditionalEffects([StatModified(SpA, i), c]));
         print_string "special attack changed "; print_int i;
         print_endline " stages";
         { mon with
           mods = {mon.mods with
                    spl_attack_mod = (mon.mods.spl_attack_mod+i)} }
       end
     else mon
  | SpD ->
     if (is_valid_modifier (mon.mods.spl_defense_mod+i)) then
       begin
         Netgraphics.add_update(AdditionalEffects([StatModified(SpD, i), c]));
         print_string "special defence changed "; print_int i;
         print_endline " stages";
         { mon with
           mods = {mon.mods with
                    spl_defense_mod = (mon.mods.spl_defense_mod+i)} }
       end
     else mon
  | Spe ->
     if (is_valid_modifier (mon.mods.speed_mod+i)) then
       begin
         Netgraphics.add_update(AdditionalEffects([StatModified(Spe, i), c]));
         print_string "speed changed "; print_int i;
         print_endline " stages";
         { mon with
           mods = {mon.mods with
                    speed_mod = (mon.mods.speed_mod+i) } }
       end
     else mon

(* Changes mon's hp by i percent, i can be positive or negative *)
let modify_hp c i mon =
  let tmp_hp = (mon.curr_hp + i*mon.max_hp/100) in
  let new_hp = if (tmp_hp > mon.max_hp) then mon.max_hp
               else if (tmp_hp < 0) then 0
               else tmp_hp
  in
  Netgraphics.add_update(UpdateSteammon (mon.species, new_hp, mon.max_hp, c));
  if new_hp = 0 then {mon with curr_hp = 0; status = None}
  else {mon with curr_hp = new_hp}


(* Heal a steammon of a particular status *)
let heal_status s c mon =
  if mon.status = Some s then
    begin
      print_endline (mon.species ^ " was healed of " ^ (string_of_status s));
      Netgraphics.add_update(AdditionalEffects[(HealedStatus s, c)]);
      {mon with status = None}
    end
  else mon

(* Heals a mon of the statuses in statuslst *)
let heal_statuses statuses c mon =
  List.fold_left (fun mn s -> heal_status s c mn) mon statuses

(* Deal dmg worth of damage to a steammon, return the steammon after
 * damage has been applied. No GUI or print updates from here. *)
let deal_dmg c dmg mon =
  if mon.curr_hp > dmg then
    (Netgraphics.add_update(UpdateSteammon (mon.species, (mon.curr_hp-dmg), mon.max_hp, c));
    {mon with curr_hp = mon.curr_hp-dmg})
  else
    (Netgraphics.add_update(UpdateSteammon (mon.species, 0, mon.max_hp, c));
    {mon with curr_hp = 0; status = None})

let num_asleep (team:team_data) =
  let (mons,_,_) = team in
    List.fold_left (fun a mon ->
                    if (mon.status = Some(Asleep)) then a+1 else a)
                         0 mons

let num_frozen (team:team_data) =
  let (mons,_,_) = team in
    List.fold_left (fun a mon ->
                    if (mon.status = Some(Frozen)) then a+1 else a)
                         0 mons

(* Apply an effect from a move to a target steammon
 * return the steammon after the effect has been applied.
 * dmg is used for recoil calculation. *)
let apply_effect ?dmg:(dmg=0) eff c mon =
  match eff with
  | InflictStatus s ->
     Netgraphics.add_update(AdditionalEffects[(InflictedStatus s, c)]);
     {mon with status = Some(s)}
  | StatModifier (s,i) -> mod_stat s i c mon
  | RecoverPercent i ->
     Netgraphics.add_update(AdditionalEffects[(Recovered i, c)]);
     modify_hp c i mon
  | Recoil i ->
     Netgraphics.add_update(AdditionalEffects[(Recoiled i, c)]);
     deal_dmg c (i*dmg/100) mon
  | DamagePercent i ->
     Netgraphics.add_update(AdditionalEffects[(Damaged i, c)]);
     modify_hp c (-i) mon
  | HealStatus statuses -> heal_statuses statuses c mon
  | RestorePP i -> restore_pp i c mon


(* Apply effects in effs from a move to a target steammon
 * return the steammon after the effects have been applied.
 * ?dmg is an optional variable to be used for recoil calculation *)
let apply_effects ?dmg:(dmg=0) effs c mon =
  List.fold_left (fun accmon eff -> apply_effect ~dmg:dmg eff c accmon) mon effs

(** Start of turn and end of turn effects on a steammon *)
(* helper function to process status effect to a steammon at the beginning
   of each step in battle phase *)
let apply_status_start c mon =
  let current_status = mon.status in
    match current_status with
    | None
    | Some Paralyzed
    | Some Poisoned
    | Some Burned -> mon
    | Some Asleep ->
       if (Random.int 100) < cWAKE_UP_CHANCE then
         begin
           Netgraphics.add_update (AdditionalEffects([(HealedStatus Asleep, c)]));
           print_string mon.species;
           print_endline " woke up!";
           {mon with status = None}
         end
       else
         begin
           Netgraphics.add_update (Message(mon.species^" is sound asleep!"));
           mon
         end
    | Some Frozen ->
       if (Random.int 100) < cDEFROST_CHANCE then
         begin
           Netgraphics.add_update
             (AdditionalEffects([(HealedStatus Frozen, c)]));
           print_string mon.species;
           print_endline " was defrosted!";
           {mon with status = None}
         end
       else
         begin
           Netgraphics.add_update
             (Message(mon.species^" is still frozen!"));
           mon
         end
    | Some Confused ->
       if (Random.int 100) < cSNAP_OUT_OF_CONFUSION then
         begin
           Netgraphics.add_update
             (AdditionalEffects([(HealedStatus Confused, c)]));
           print_string mon.species;
           print_endline " snapped out of its confusion!";
           {mon with status = None}
         end
       else
         begin
           Netgraphics.add_update (Message(mon.species^" is still confused!"));
           mon
         end

(* helper function to process status effect to a steammon at the end
   of each step in battle phase *)
let apply_status_end c mon =
  let current_status = mon.status in
    match current_status with
    | None
    | Some Asleep
    | Some Frozen
    | Some Paralyzed
    | Some Confused -> mon
    | Some Poisoned ->
       let max_hp = (float_of_int mon.max_hp) in
       let psn_dmg = int_of_float (max_hp*.cPOISON_DAMAGE) in
       let monnew = deal_dmg c psn_dmg mon in
       Netgraphics.add_update
         (AdditionalEffects([(InflictedStatus Poisoned, c);
                             (DamagedByStatus (psn_dmg, Poisoned), c)]));
       print_string mon.species;
       print_endline " is damaged by poison!";
       monnew
    | Some Burned ->
       let max_hp = (float_of_int mon.max_hp) in
       let brn_dmg = int_of_float (max_hp*.cBURN_DAMAGE) in
       let monnew = deal_dmg c brn_dmg mon in
       Netgraphics.add_update
         (AdditionalEffects([(InflictedStatus Burned, c);
                             (DamagedByStatus (brn_dmg, Poisoned), c)]));
       print_string mon.species;
       print_endline " is hurt by its burn!";
       monnew

let replace_mon_lst mon_new mons =
  let tmpmons =
    begin
    List.fold_left
      (fun a mon -> if mon.species = mon_new.species
                  then mon_new::a else mon::a) [] mons
    end
  in List.rev tmpmons

let replace_mon mon_new team=
  let (mons, inv, cre) = team in
    let monsnew = replace_mon_lst mon_new mons in
    (monsnew, inv, cre)

let update_mons c monpair gs =
  let (rteam, bteam) = gs in
  let (active_mon, opponent_mon) = monpair in
  match c with
  | Red -> (replace_mon active_mon rteam, replace_mon opponent_mon bteam)
  | Blue -> (replace_mon opponent_mon rteam, replace_mon active_mon bteam)

(** Functions for switching steammon*)
let switch_helper monstr mons =
  List.fold_left
    (fun a mon -> if mon.species = monstr then mon::a
                  else List.rev(mon::(List.rev a))) [] mons

(* Method to switch in steammon represented by string monstr from team of
 * color c. T *)
let switch_in monstr c gs =
  let (rteam, bteam) = gs in
  match c with
  | Red -> let (rmons, rinv, rcre) = rteam in
           let rmonsnew = switch_helper monstr rmons in
           let rleadstr = (List.hd rmonsnew).species in
           Netgraphics.add_update (SetChosenSteammon rleadstr);
           print_endline ("Red sent out " ^ rleadstr ^ "!");
           ((rmonsnew, rinv, rcre), bteam)
  | Blue -> let (bmons, binv, bcre) = bteam in
            let bmonsnew = switch_helper monstr bmons in
            let bleadstr = (List.hd bmonsnew).species in
            Netgraphics.add_update (SetChosenSteammon bleadstr);
            print_endline ("Blue sent out " ^ bleadstr ^ "!");
            (rteam, (bmonsnew, binv, bcre))

(** Move implementation functions *)
(* Get the lead steammon for use_move *)
let get_lead_steammon gs c =
  let (rteam, bteam) = gs in
  match c with
  | Red -> let (rmons, _, _) = rteam in (List.hd rmons)
  | Blue -> let (bmons, _, _) = bteam in (List.hd bmons)

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

(* Precondition: mov.pp_remaining > 0 *)
let decrease_pp_move (mov:move): move =
  {mov with pp_remaining = mov.pp_remaining -1}

(* Decreases the pp of a move mov of a steammon by 1,
 * Does nothing if the move is not in a steammon's list of moves
 * because of Struggle or AttackSelf *)
let decrease_pp (mov:move) (mon: steammon): steammon =
  if mov.name = "Struggle" then
    mon
  else
    let tmpmov = decrease_pp_move mov in
    if mon.first_move = mov then
      {mon with first_move = tmpmov}
    else if mon.second_move = mov then
      {mon with second_move = tmpmov}
    else if mon.third_move = mov then
      {mon with third_move = tmpmov}
    else if mon.fourth_move = mov then
      {mon with fourth_move = tmpmov}
    else
      mon

(* Determine the move actually used by the steammon, i.e. check if
 * it has sufficient pp and return the Struggle option if not enough pp
 * is available.*)
let actual_move (movop: move option): move option =
  match movop with
  | None -> None
  | Some mov -> if mov.pp_remaining = 0 then
                  begin
                  print_endline ("No pp for " ^  mov.name ^ " left!");
                  Some (Table.find move_table "Struggle")
                  end
                else movop

let effs_wo_status s effs =
  List.fold_left (fun a eff -> if eff = InflictStatus s then a else eff::a ) [] effs

let effs_wo_slpfrz any_asleep any_frozen effs =
  if (any_asleep && any_frozen) then
    (effs_wo_status Frozen (effs_wo_status Asleep effs))
  else if any_asleep then
    (effs_wo_status Asleep effs)
  else if any_frozen then
    (effs_wo_status Frozen effs)
  else
    effs

(* Return the monpair after effects from a move have been applied to steammon of team color c *)
let handle_moneffs ?damage:(damage=0) any_asleep any_frozen effects c monpair =
  let (active_mon, opponent_mon) = monpair in
  match effects with
  | None -> (active_mon, opponent_mon)
  | Some (effs, t, i) ->
     let rand2 = Random.int 100 in
     if (rand2 < i) then
       begin
         match t with
         | User ->
            let effsnew = effs_wo_slpfrz any_asleep any_frozen effs in
            let active_monnew = apply_effects ~dmg:damage effsnew c active_mon in
            (active_monnew, opponent_mon)
         | Opponent ->
            let opponent_monnew = apply_effects effs (invert_color c) opponent_mon in
            (active_mon, opponent_monnew)
       end
     else
       (active_mon, opponent_mon)


(* converts effects of a move to target team of color c to effect of a move_result.
 * Since move cannot cause DamageByStatus, this result is ignored *)
let effect_result_of_effect (c: color) (eff_lst: effect list): (effect_result*color) list =
    List.fold_left (
      fun a x ->
        match x with
       | InflictStatus s -> (InflictedStatus s, c)::a
       | StatModifier (s,i) -> (StatModified (s,i), c)::a
       | RecoverPercent i -> (Recovered i, c)::a
       | Recoil i -> (Recoiled i, c)::a
       | DamagePercent i -> (Damaged i, c)::a
       | HealStatus statuses -> List.fold_left (fun acc sts -> (HealedStatus sts, c)::acc) a statuses
       | RestorePP i -> (RestoredPP i, c)::a
    ) [] eff_lst


(* returns the move_result of a move in order to add the gui update for Move *)
let get_move_result (mov_opt: move option) (result: hit_result) (cpair: color*color) (monpair: steammon*steammon)  : move_result =
  match mov_opt with
  | None -> failwith "there is no move!"
  | Some mov ->
                let (active_mon, target_mon) = monpair in
                  let (c_from, c_to) = cpair in
                  {
                  name = mov.name;
                  element = mov.element;
                  from = c_from;
                  toward = c_to;
                  damage = det_damage active_mon mov target_mon;
                  hit = result;
                  effectiveness = (fst (eff_and_multiplier target_mon mov));
                  effects = match mov.effects with
                            | None -> []
                            | Some (eff_lst,_,_) -> effect_result_of_effect c_to eff_lst

                  }


(* returns the new active_monnew, opponent_monnew pair
 * after the move mov occurs *)
let handle_monmov any_asleep any_frozen mov c_from monpair =
  let (active_mon, opponent_mon) = monpair in
  let active_monnew = decrease_pp mov active_mon in
  let rand = Random.int 100 in
  print_endline (active_mon.species ^ " used " ^ mov.name ^ "!" );
  if (rand < mov.accuracy && mov.target = Opponent) then
    let d = det_damage active_mon mov opponent_mon in
    let opponent_monnew = deal_dmg (invert_color c_from) d opponent_mon in
    print_string (active_mon.species ^ " hit " ^
                    opponent_mon.species ^  " with " ^ mov.name ^ " for ");
    print_int d; print_endline " damage!";
    Netgraphics.add_update(Move(get_move_result (Some mov) Hit (c_from, invert_color c_from) monpair));
    handle_moneffs ~damage:d any_asleep any_frozen mov.effects c_from (active_monnew, opponent_monnew)
  else if (mov.target = User) then
    begin
      Netgraphics.add_update(Move(get_move_result (Some mov) Hit (c_from, c_from) monpair));
      handle_moneffs false false mov.effects c_from (active_monnew, opponent_mon)
    end
  else
    begin
      print_string mov.name; print_string "missed!";
      (active_monnew, opponent_mon)
    end

let team_of_color c gs =
  let (rteam, bteam) = gs in
    match c with
    | Red -> rteam
    | Blue -> bteam

(* handles a move that occurs, movop is the move option
 * returned by actual_move. c_from is the team color that has the move
 * invert_color is called from util.ml *)
let handle_attack movop c_from gs =
  let opp_color = (invert_color c_from) in
  let active_mon = get_lead_steammon gs c_from in
  let opponent_mon = get_lead_steammon gs opp_color in
  let any_asleep = (num_asleep (team_of_color opp_color gs) > 0) in
  let any_frozen = (num_frozen (team_of_color opp_color gs) > 0) in
  (* let opponent_monstr = opponent_mon.species in
      let active_monstr = active_mon.species in *)
  match movop with
  | None -> print_endline "Invalid move selected."; gs
  | Some mov ->
     let (active_monnew, opponent_monnew) =
       handle_monmov any_asleep any_frozen mov c_from (active_mon, opponent_mon)  in
     let active_monnew2 = apply_status_end c_from active_monnew in
     let (rteam_new, bteam_new) = update_mons c_from (active_monnew2, opponent_monnew) gs in
     (rteam_new, bteam_new)


let handle_attackself (c:color) (gs:game_status_data): game_status_data =
  let (rteam, bteam) = gs in
  let active_mon = get_lead_steammon gs c in
  let active_monstr = active_mon.species in
  let attackself = (Table.find move_table "SelfAttack") in
  let rand = Random.int 100 in
  print_endline (active_mon.species ^ " used " ^ attackself.name ^ "!" );
  if (rand < attackself.accuracy) then
    let selfdamage = det_damage active_mon attackself active_mon in
    let active_monnew = deal_dmg c selfdamage active_mon in
    print_string (active_monstr ^ " hit itself " ^ " with " ^ attackself.name ^ " for ");
    print_int selfdamage; print_endline " damage!";
    Netgraphics.add_update(Move(get_move_result (Some attackself) Hit (c, c) (active_mon, active_mon)));
    let rteamnew = replace_mon active_monnew rteam in
    (rteamnew, bteam)
  else
    (Netgraphics.add_update(Move(get_move_result (Some attackself) Miss (c, c) (active_mon, active_mon)));
    print_endline (active_monstr ^ " missed its SelfAttack!");
    gs;)

(* Function to use a move represented by string move from the active Steammon of the team of
 * color c. Takes in the current game state and returns an updated game
 * state with both team_data updated based on the move c *)
let use_move (movstr: string) (c:color) (gs: game_status_data): game_status_data =
  let active_mon = apply_status_start c (get_lead_steammon gs c) in
  if active_mon.curr_hp = 0 then gs
  else
    let active_monstr = active_mon.species in
    let movop = get_move_from_steammon active_mon movstr in
    let actual_mov = actual_move movop in
    let rand = Random.int 100 in
    match active_mon.status with
    | Some (Frozen) ->
       (* GUI update for failed status *)
       Netgraphics.add_update(Move(get_move_result actual_mov (Failed Frozen) (c, c) (active_mon, active_mon)));
       print_string active_monstr; print_endline " is frozen and cannot attack!"; gs
    | Some (Asleep) ->
       (* GUI update for failed status *)
       Netgraphics.add_update(Move(get_move_result actual_mov (Failed Asleep) (c, c) (active_mon, active_mon)));
       print_string active_monstr; print_endline " is asleep and cannot attack!"; gs
    | Some (Confused) ->
       print_string active_monstr; print_endline " is confused!";
       if (rand < cSELF_ATTACK_CHANCE) then
         begin
           (* GUI update for special move attackself *)
           print_string active_monstr;
           print_endline " attacked itself in its confusion!";
           handle_attackself c gs
         end
       else
         handle_attack actual_mov c gs
    | Some (Paralyzed) ->
       print_string active_monstr; print_endline " is paralyzed!";
       if (rand < cPARALYSIS_CHANCE) then
         begin
           (* GUI update for failed status *)
           Netgraphics.add_update(Move(get_move_result actual_mov (Failed Paralyzed) (c, c) (active_mon, active_mon)));
           print_string active_monstr; print_endline " can't move!"; gs
         end
       else
         handle_attack actual_mov c gs
    | Some (Poisoned)
    | Some (Burned)
    | None ->  handle_attack actual_mov c gs

let add_update_Item_HealedStatus (thing: string) (mon: steammon) (c: color) =
  begin
    match mon.status with
    | None -> ()
    | Some sts ->
       Netgraphics.add_update (Item(thing, HealedStatus sts, c, mon.species));
  end

(* applies item to steammon and returns the steammon after item is applied *)
let apply_item thing c mon =
  begin
    match thing with
    | Ether -> Netgraphics.add_update
                 (Item ("Ether", RestoredPP 5, c, mon.species));
               apply_effect (RestorePP 5) c mon
    | MaxPotion -> if (mon.curr_hp = 0) then mon else
                    (Netgraphics.add_update
                       (Item ("MaxPotion",
                          Recovered (mon.max_hp - mon.curr_hp), c,
                            mon.species));
                    modify_hp c 100 mon)
    | Revive -> if mon.curr_hp = 0 then
                  begin
                    Netgraphics.add_update
                      (Item ("Revive",
                             Recovered (mon.max_hp / 2 - mon.curr_hp),
                             c, mon.species));
                    modify_hp c 50 mon
                  end
                else mon
    | FullHeal -> add_update_Item_HealedStatus "FullHeal" mon c;
                  {mon with status = None}
    | XAttack ->  mod_stat Atk 1 c mon
    | XDefense -> mod_stat Def 1 c mon
    | XSpeed -> mod_stat SpD 1 c mon
  end


(* helper function, returns the position of each item in the inventory. The first item has position 0 *)
let int_of_item (thing: item) : int =
  match thing with
  | Ether -> 0
  | MaxPotion -> 1
  | Revive -> 2
  | FullHeal -> 3
  | XAttack -> 4
  | XDefense -> 5
  | XSpeed -> 6

(* helper function for removing one used item from the inventory. returns the changed inventory *)
let remove_item  (thing: item) (inv: inventory) =
  let tmplst = List.fold_left
                 (fun a x -> if (List.length a = int_of_item thing)
                             then (x-1)::a else x::a) [] inv
  in List.rev tmplst

(* Get the steammon from a list given its species *)
let get_mon (monstr:string) (mons: steammon list): steammon option =
  List.fold_left (fun a mon -> if (mon.species = monstr) then Some mon else a) None mons

(* helper function for use_item *)
let use_item_team thing monstr c team =
  let (mons, inv, cre) = team in
  let monop = get_mon monstr mons in
  match monop with
  | None -> team
  | Some mon ->
     if (List.mem mon mons) then
       let mon_new = apply_item thing c mon in
       let mons_new = replace_mon_lst mon_new mons in
       let inv_new = remove_item thing inv in
       (mons_new, inv_new, cre)
     else
       team

(* lets team of color c use item thing on steammon mon and
 * return the game_state afterwards *)
let use_item (thing: item) (monstr: string) (c:color)
             (gs:game_status_data): game_status_data =
  let (rteam, bteam) = gs in
  match c with
  | Red -> (let (ms,_,_) = rteam in
            match get_mon monstr ms with
              | Some m -> if m.curr_hp = 0 then gs
                          else (use_item_team thing monstr c rteam , bteam)

              | None -> failwith "nevah" )

  | Blue -> let (ms,_,_) = bteam in
            match get_mon monstr ms with
              | Some m -> if m.curr_hp = 0 then gs
                          else (rteam, use_item_team thing monstr c bteam)
              | None -> failwith "nevah"
