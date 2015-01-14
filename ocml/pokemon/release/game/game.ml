open Definitions
open Util
open Constants
open Netgraphics
open Initialization
open State

type game = {mutable game_state: game_status_data}

let game_datafication g =
  g.game_state

let game_from_data game_data =
    {game_state = game_data}

let game_stage = ref 1
let l_pick = ref None

(* Sends an UpdateSteammon update for a steammon added to a team of color *)
let add_update_steammon (mon:steammon) (c:color): unit =
  let monupdate = (mon.species, mon.curr_hp, mon.max_hp, c) in
  Netgraphics.add_update (UpdateSteammon (monupdate))

(* Processes action a by team c according to game state gs and returns an
 * updated game state *)
let process_action  (a:action) (c:color) (gs:game_status_data) =
  match a with
  | SwitchSteammon monstr -> switch_in monstr c gs
  | UseItem (item, monstr) -> use_item item monstr c gs
  | UseMove movstr -> use_move movstr c gs
  | SelectStarter monstr -> switch_in monstr c gs
  | _ -> print_endline "bot is not sending a battle Action "; gs

(* Randomly returns color either Red or Blue *)
let random_color () =
  let rand = Random.int 100 in
  if (rand < 50) then
    (Netgraphics.add_update(SetFirstAttacker Red);
    Red)
  else
    (Netgraphics.add_update(SetFirstAttacker Blue);
    Blue)

(* Determine the speed of a steammon for determine_priority *)
let speed_of_mon (mon:steammon) =
  let paralyze_factor = (if mon.status = Some Paralyzed then cPARALYSIS_SLOW else 1) in
  (float_of_int mon.speed) *. (multiplier_of_modifier mon.mods.speed_mod) /. (float_of_int paralyze_factor)

(* Returns the color of the team with the fastest Steammon according to the current game state.
 * If the Steammon have equal speed, returns a random color *)
let determine_priority (gs:game_status_data) (r_action:action) (b_action:action): color =
  let ((r_mons, r_inv, r_cre), (b_mons, b_inv, b_cre)) = gs in
  let r_mon = (List.hd r_mons) in
  let b_mon = (List.hd b_mons) in
  let r_speed = speed_of_mon r_mon in
  let b_speed = speed_of_mon b_mon in
  let faster =
    if (r_speed > b_speed) then
      (Netgraphics.add_update(SetFirstAttacker Red);
      Red)
    else if (r_speed = b_speed) then random_color ()
    else
      (Netgraphics.add_update(SetFirstAttacker Blue);
      Blue)
  in
  faster

(* Returns true if all Steammon in the given team have fainted *)
let is_done (team:team_data) =
  let (mons, inv, cre) = team in
  List.fold_left (fun acc mon -> if (mon.curr_hp > 0) then false else acc) true mons

(* Generates the output for handle step at the very end of a turn given a processed game status.
 * Handles wins, ties, and ongoing battles, sending the appropriate requests to the players *)
let generate_output (gs:game_status_data): game_output =
  (* Returns the output for handle step when at least one team's Steammon have
   * all fainted. r indicates whether the red team has all fainted, and b indicates
   * whether the blue team has all fainted *)
  let endgame_output (r:bool) (b:bool): game_output =
    match (r, b) with
    | (true, true) -> (Some(Tie), gs, None, None)
    | (true, false) -> (Some(Winner Blue), gs, None, None)
    | (false, true) -> (Some(Winner Red), gs, None, None)
    | (false, false) -> failwith "Error in endgame_output" in
  (* Returns the output for handle step when the battle is ongoing. Looks for
  fainted active Steammon to send StarterRequests and otherwise sends ActionRequests *)
  let battle_output (red:team_data) (blue:team_data): game_output =
    let (red_mons, r_inv, r_i) = red in
    let (blue_mons, b_inv, b_i) = blue in
    let red_lead = (List.hd red_mons) in
    let blue_lead = (List.hd blue_mons) in
    let red_fainted = (red_lead.curr_hp = 0) in
    let blue_fainted = (blue_lead.curr_hp = 0) in
    match (red_fainted, blue_fainted) with
    | (true, true) ->
       print_endline ("Red's " ^ red_lead.species ^ " fainted!");
       print_endline ("Blue's " ^ blue_lead.species ^ " fainted!");
       (None, gs, Some(Request(StarterRequest gs)), Some(Request(StarterRequest gs)))
    | (true, false) ->
       print_endline ("Red's " ^ red_lead.species ^ " fainted!");
       (None, gs, Some(Request(StarterRequest gs)), None)
    | (false, true) ->
              print_endline ("Blue's " ^ blue_lead.species ^ " fainted!");
       (None, gs, None, Some(Request(StarterRequest gs)))
    | (false, false) ->
       (None, gs, Some(Request(ActionRequest gs)), Some(Request(ActionRequest gs))) in
  let (red_team, blue_team) = gs in
  let red_done = is_done red_team in
  let blue_done = is_done blue_team in
  if (red_done || blue_done) then endgame_output red_done blue_done
  else battle_output red_team blue_team

(* Helper function for handle_send_teamname.
 * updates game_stage to 2 now that teamnamerequests are done.*)
let handle_send_teamname_helper ((rname,bname): string*string) (gs: game_status_data): game_output =
  let mvlst = hash_to_list move_table in
  let monlst = hash_to_list mon_table in
  Netgraphics.send_update (InitGraphics (rname,bname));
  print_endline ("Red is: " ^ rname);
  print_endline ("Blue is: " ^ bname);
  game_stage:= 2;
  if (Random.int 100 < 50) then
    ((l_pick := Some Red);
    (None, gs, Some (Request (PickRequest (Red, gs, mvlst, monlst))), None))
  else
    ((l_pick := Some Blue);
    (None, gs, None, Some (Request (PickRequest (Blue, gs, mvlst, monlst)))))

(* helper function for handle_step to handle a SendTeamName request *)
let handle_send_teamname (gs: game_status_data) (ra:command) (ba:command): game_output =
    match (ra,ba) with
    | (Action(SendTeamName rname), Action(SendTeamName bname)) ->
       handle_send_teamname_helper (rname, bname) gs
    | (Action(SendTeamName rname), DoNothing) ->
       handle_send_teamname_helper (rname, "Blue") gs
    | (DoNothing, Action(SendTeamName bname)) ->
       handle_send_teamname_helper ("Red", bname) gs
    | (DoNothing, DoNothing) ->
       handle_send_teamname_helper ("Red", "Blue") gs
    | (_,_) ->
       print_endline "Bot is not sending ActionRequests";
       handle_send_teamname_helper ("Red", "Blue") gs


(* Handles a pick_request given commands that are in response
 * to a PickRequest, usually involves PickSteammon actions
 * updates game_stage to 3 when all pick_requests are done *)
let rec handle_pick_request  (gs: game_status_data) (ra:command) (ba:command): game_output =
    let (rteam, bteam) = gs in
    let mvlst = hash_to_list move_table in
    let monlst = hash_to_list mon_table in
    match (ra,ba) with
    | (Action(PickSteammon monstr), DoNothing) ->
       begin
         l_pick := Some Red;
         Netgraphics.add_update(Message("Red wants a steammon"));
         print_endline ("Red wants a " ^ monstr);
         let rteamnew = pick_steammon monstr rteam in
         let gsnew = (rteamnew, bteam) in
         let (rmons, ritems, rcre) = rteamnew in
         let monnew = List.hd rmons in
         add_update_steammon monnew Red;
         if ((num_steammons(rteamnew) = num_steammons(bteam) && num_steammons(bteam) < cNUM_PICKS)) then
           (None, gsnew, Some (Request(PickRequest (Red, gsnew, mvlst, monlst))), None)
         else if (num_steammons(rteamnew) > num_steammons(bteam) && num_steammons(bteam)< cNUM_PICKS ) then
           (None, gsnew, None, Some (Request (PickRequest (Blue, gsnew, mvlst, monlst))))
         else
           begin
             print_int (num_steammons(rteamnew));
             print_endline " steammons in red team";
             print_int (num_steammons(bteam));
             print_endline " steammons in blue team";
             game_stage:= 3;
             (None, gsnew, Some (Request (PickInventoryRequest(gsnew))), Some (Request (PickInventoryRequest(gsnew))))
           end
       end
    | (DoNothing, Action(PickSteammon monstr)) ->
       begin
         l_pick := Some Blue;
         print_endline ("Blue wants a " ^ monstr);
         let bteamnew = pick_steammon monstr bteam in
         let gsnew = (rteam, bteamnew) in
         let (bmons, bitems, rcre) = bteamnew in
         let monnew = List.hd bmons in
         add_update_steammon monnew Blue;
         if ((num_steammons(rteam) = num_steammons(bteamnew) && num_steammons(rteam) < cNUM_PICKS)) then
           (None, gsnew, None, Some (Request (PickRequest (Blue, gsnew, mvlst, monlst))))
         else if (num_steammons(rteam) < num_steammons(bteamnew) && num_steammons(rteam) < cNUM_PICKS) then
           (None, gsnew, Some (Request (PickRequest (Red, gsnew, mvlst, monlst))), None)
         else
           begin
             print_int (num_steammons(rteam));
             print_endline " steammons in red team";
             print_int (num_steammons(bteamnew));
             print_endline " steammons in blue team";
             game_stage:= 3;
             (None, gsnew, Some (Request (PickInventoryRequest(gsnew))), Some (Request (PickInventoryRequest(gsnew))))
           end
       end
    | (DoNothing, DoNothing) -> begin
       match !l_pick with
      | Some Red -> if (num_steammons(rteam) = num_steammons(bteam))  then
                 handle_pick_request gs (Action(PickSteammon ((lc_steammon ()).species))) DoNothing
               else handle_pick_request gs DoNothing (Action(PickSteammon ((lc_steammon ()).species)) )
      | Some Blue -> if (num_steammons(rteam) = num_steammons(bteam))  then
                       handle_pick_request gs DoNothing (Action(PickSteammon ((lc_steammon ()).species)) )
                     else handle_pick_request gs (Action(PickSteammon ((lc_steammon ()).species))) DoNothing
      |_ -> failwith "wrong"
      end
    | (_,_) -> failwith "to handle exception"

(* Picks an inventory for the two teams given the inventories they would pick,
 * exception handling by handle_pickinv*)
let handle_pickinv_helper (gs:game_status_data)
                          (rinv: inventory) (binv: inventory): game_status_data =
  let (rteam, bteam) = gs in
  let rteamnew = pick_inventory rinv rteam in
  let bteamnew = pick_inventory binv bteam in
  let (rmons, rinvnew, rcre) = rteamnew in
  let (bmons, binvnew, bcre) = bteamnew in
  print_string "Red has items: "; print_inv rinvnew; print_endline "";
  print_string "Blue has items: "; print_inv binvnew; print_endline "";
  game_stage:= 4;
  (rteamnew, bteamnew)

let handle_pickinv_request (gs: game_status_data)
                           (ra:command) (ba:command): game_output =
  match (ra,ba) with
  | (Action(PickInventory rinv), Action(PickInventory binv)) ->
     let gsnew = handle_pickinv_helper gs rinv binv in
     (None, gsnew, Some(Request(StarterRequest(gsnew))), Some(Request(StarterRequest(gsnew))) )
  | (Action(PickInventory rinv), DoNothing) ->
     let gsnew = handle_pickinv_helper gs rinv cDEFAULT_INV in
     (None, gsnew, Some(Request(StarterRequest(gsnew))), Some(Request(StarterRequest(gsnew))) )
  | (DoNothing, Action(PickInventory binv)) ->
     let gsnew = handle_pickinv_helper gs cDEFAULT_INV binv in
     (None, gsnew, Some(Request(StarterRequest(gsnew))), Some(Request(StarterRequest(gsnew))) )
  | (DoNothing, DoNothing) ->
     let gsnew = handle_pickinv_helper gs cDEFAULT_INV cDEFAULT_INV in
     (None, gsnew, Some(Request(StarterRequest(gsnew))), Some(Request(StarterRequest(gsnew))) )
  | (_,_) ->
     print_endline "Bot is not sending ActionRequests...";
     let gsnew = handle_pickinv_helper gs cDEFAULT_INV cDEFAULT_INV in
     game_stage:= 4;
     (None, gsnew, Some(Request(StarterRequest(gsnew))), Some(Request(StarterRequest(gsnew))) )

let handle_battle_action (gs: game_status_data)
                           (ra:command) (ba:command): game_output =
  match (ra, ba) with
  | (Action a, DoNothing) -> generate_output (process_action a Red gs)
  | (DoNothing, Action b) -> generate_output (process_action b Blue gs)
  | (Action a, Action b) ->
     let status =
        if ((determine_priority gs a b) = Red) then
          process_action b Blue (process_action a Red gs)
        else
          process_action a Red (process_action b Blue gs)
     in
      generate_output status
  | (DoNothing, DoNothing) -> generate_output gs
  | (_,_) -> print_endline "Bot is not sending ActionRequests..."; generate_output gs




(* handle_step gs ra ba is used to generate the next state of the game.
 * gs is the current game state entering this turn.
 * ra and ba are the Action commands sent by the red team and the blue
 * team, respectively. If ra or ba are not Action(action) commands,
 * the game will ignore the command given.
 * handle_step outputs game_output (gr, gs, rr, br) where:
 * gr is the result of the game. Output None if no winner was determined.
 * gs is the state of the game after handle_step ended.
 * rr is an option containing the request for the red team
 * br is an option containing the request for the blue team
 * None indicates that the team should respond with DoNothing
 * val handle_step: game -> command -> command -> game_output
 *)
let handle_step g ra ba =
  let gs = game_datafication g in
  (* Random number  generator MUST be initialized *)
  Random.self_init ();
  match (!game_stage) with
  | 1 -> handle_send_teamname gs ra ba
  | 2 -> handle_pick_request gs ra ba
  | 3 -> handle_pickinv_request gs ra ba
  | 4 -> handle_battle_action gs ra ba
  | _ -> failwith "WHAT THE ZARDOZ HAPPENED HERE?"


(* init_game generates a blank copy of the game, returning (gs * r1 * r2
 * al * sl).
 * gs is a game with inventories initialized, and neither player with any steammon.
 * r1 is the first request sent out to the red team
 * r2 is the first request sent out to the blue team
 * al is the list of all attacks, as read from moves.csv
 * sl is the list of all steammon as read from steammon.csv
 * val init_game: unit -> game * request * request * move list * steammon list
 *)
let init_game () =
  init_pool "moves.csv" "steammon.csv";
  let al = hash_to_list move_table in
  let sl = hash_to_list mon_table in
  let r1 = TeamNameRequest in
  let r2 = TeamNameRequest in
  let rteam = cINITIAL_TEAM in
  let bteam = cINITIAL_TEAM in
  let gs = game_from_data (rteam, bteam) in
  (gs, r1, r2, al, sl)
