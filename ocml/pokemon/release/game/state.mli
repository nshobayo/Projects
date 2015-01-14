open Definitions

(* state.ml contains functions for working with game_status_data, which represents the
 * state of the game.
 * For clarity, please use the following abbreviations (more to be added):
 * mon - a variable of type steammon
 * mons - a steammon list used in representing a team
 * inv - an int list to represent the inventory of a team
 * gs - a variable of type game_status_data
 * team - a variable of type team_data
 * suffix with r, b, new, etc whenever necessary *)

val cDEFAULT_INV: inventory

val cITEM_COSTS: inventory

val cINITIAL_TEAM: team_data

(** Draft phase functions *)
(* Lets the player of a team pick a steammon.
 * pick_steammon monstr team tries to add the steammon represented by monstr
 * into team. monstr must be a string in the steammon_pool
 * team is of type team_data *)
val lc_steammon: unit -> steammon 
val pick_steammon: string -> team_data -> team_data

(* Returns the number of steammons in a team of type team_data*)
val num_steammons: team_data -> int

(** inventory phase functions *)
(* return the cost of an inventory of items *)
val inv_cost: inventory -> int

(* helper function for a team to handle PickInventory action command
 * pick_inventory invnew team
 * returns the team with the inventory as invnew if invnew has
 * total cost less than cINITIAL_CASH *)
val pick_inventory: inventory ->  team_data ->  team_data

val print_inv: inventory -> unit

(** Functions to help implement apply_effect on a steammon *)
(* restore_pp_move i c mov restores i pp to a move mov which
 * is on a steammon of color c. *)
val restore_pp_move: int -> color -> move -> move

(* restore_pp_move i c mon restores i pp to a move mov which
 * is on a steammon of color c. *)
val restore_pp: int -> color -> steammon -> steammon

(* mod_stat s i c mon modifies the stat s of an steammon
 * on the team of color c by an integer i. *)
val mod_stat: stat -> int -> color -> steammon -> steammon

(* modify_hp c i mon modifies the mon's hp by i
 * percent where i can be positive or negative.
 * mon is of team color c *)
val modify_hp: color -> int -> steammon -> steammon

(* heal_status s c mon heals a steammon of a particular status *)
val heal_status: status -> color -> steammon -> steammon

(* heal_status statuses c mon heals a steammon of a particular status
 * if the steammon has a status that is in statuses. *)
val heal_statuses: status list -> color -> steammon -> steammon

(* deal_dmg c dmg mon deals dmg of damage to a steammon mon of team color c and returns
 * the steammon after the damage is dealt *)
val deal_dmg: color -> int -> steammon -> steammon

(* apply_effect ~dmg:d eff c mon returns a mon after effect eff is applied
 * dmg is an optional variable to be used for recoil calculation *)
val apply_effect: ?dmg:int -> effect -> color -> steammon -> steammon

(* apply_effects ~dmg:d effs c mon returns a mon after all effects effs is applied
 * ?dmg is an optional variable to be used for recoil calculation *)
val apply_effects: ?dmg:int -> effect list -> color -> steammon -> steammon

(** start and end of turn status effects *)
(* apply_status_start c mon applies start of turn status effects to mon*)
val apply_status_start: color -> steammon -> steammon

(* apply_status_end c mon applies end of turn status effects to mon*)
val apply_status_end: color -> steammon -> steammon

(* replace_mon_lst mon_new mons
 * updates mon in a list - used for updating mons and inv together *)
val replace_mon_lst: steammon -> steammon list -> steammon list

(* replace_mon mon_new team replaces the original mon with mon_new
 * in team - used for updating teams*)
val replace_mon: steammon -> team_data -> team_data

(* update_mons c monpair gs updates (active_mon, opponent_mon) after
 * a battle move. c is the team of the active_mon *)
val update_mons: color -> steammon*steammon -> game_status_data -> game_status_data

(* Helper function for switch_in. switch_helper monstr mons
 * takes mons and returns a mons with
 * the steammon represented by monstr at the head of the list *)
val switch_helper: string -> steammon list -> steammon list

(* switch_in monstr c gs takes in the current game state
 * and returns an updated game state with c's team_data
 * updated so that steammon mon is at the head of its steammon list*)
val switch_in: string -> color -> game_status_data -> game_status_data

(** move implementation functions *)
(* get_lead_steammon gs c returns the lead steammon of
 * team color c. *)
val get_lead_steammon: game_status_data -> color -> steammon

(* Find the number of asleep steammon in a team *)
val num_asleep: team_data -> int
(* Find the number of frozen steammon in a team *)
val num_frozen: team_data -> int
(* Returns a list of effects without a particular
 * InflictStatus (sleep or frozen) for sleep clause implementation *)
val effs_wo_status: status -> effect list -> effect list
(* Returns a list of effects after applying the sleep, freeze clause *)
val effs_wo_slpfrz: bool -> bool -> effect list -> effect list

(* handle_moneffs any_asleep any_frozen ~damage:d effects c monpair
 * returns a monpair after move effects have been applied *)
val handle_moneffs: ?damage:int -> bool -> bool -> (effect list* target * int) option ->
                    color -> steammon*steammon ->  steammon*steammon

(* Gets the team data being referred to by a color *)
val team_of_color: color -> game_status_data -> team_data

(* handle_monmov any_asleep any_frozen mov c_from monpair returns a steammon pair after an attack occurs. *)
val handle_monmov: bool -> bool -> move -> color -> steammon*steammon -> steammon*steammon

(* handles a move that occurs, movop is the move option
 * returned by actual_move. c is the team color that has the move *)
val handle_attack:  move option -> color -> game_status_data ->
                    game_status_data

(* handles a move that occurs. string is a move's name*)
val use_move: string -> color ->  game_status_data -> game_status_data

(* switch_in monstr c gs switches the mon represented by monstr in the team
 * of color c to the front of c's steammon list. Returns the new game_status_data *)
val switch_in: string -> color -> game_status_data -> game_status_data

(** important functions to deal with attacks *)
(* det_damage active_mon mov opponent_mon returns the damage dealt by
 * by active_mon using mov on opponent_mon *)
val det_damage: steammon -> move -> steammon -> int

(* used for process_action in game.ml *)
val use_move:  string -> color ->  game_status_data -> game_status_data

val apply_item: item -> color -> steammon -> steammon

val use_item_team: item -> string -> color -> team_data -> team_data

val use_item: item -> string -> color ->  game_status_data -> game_status_data
