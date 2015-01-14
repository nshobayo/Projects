open Definitions
open Util
open Constants
open Netgraphics
open Initialization
open State


(* applies item to steammon and returns the steammon after item is applied *)
let apply_item (thing:item) (mon: steammon): steammon =
  begin
    match thing with
    |Ether -> apply_effect (RestorePP 5) mon
    |MaxPotion -> {mon with curr_hp = mon.max_hp}
    |Revive -> if mon.curr_hp = 0 then
                 {mon with curr_hp = (mon.max_hp / 2);
                           status = None}
               else mon
    |FullHeal -> {mon with status = None}
    |XAttack -> {mon with attack = (mon.attack +1)}
    |XDefense -> {mon with defense = (mon.defense+1)}
    |XSpeed -> {mon with speed = (mon.speed+1)}
  end

(* helper function for the item effect on target steammon.
 * after the item takes effect on the target steammon,
 * returns the steammon list with
 * target steammon in its original position in list.
 * if the target steammon doesn't
 * exist in list, return original list
 * replace_mon is called from state.ml*)
let item_effect (mons: steammon list) (thing: item) (mon: steammon): steammon list =
  if not (List.mem mon mons) then mons else
    let mon_new = apply_item thing mon
    in replace_mon mons mon_new

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
let inv_after_item  (thing: item) (inv: inventory) =
  let tmplst = List.fold_left
                 (fun a x -> if (List.length a = int_of_item thing)
                             then (x-1)::a else x::a) [] inv
  in List.rev tmplst
