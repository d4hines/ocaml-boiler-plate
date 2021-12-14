type hash = string
type level = int
type step

module VM = struct
  type action = unit
  type t = unit
  let execute_step : t -> t = assert false
  let has_halted : t = assert false

  let apply : t -> action -> t= assert false

  let hash : t -> hash= assert false
  let step : t -> step= assert false
end 

type state = {
  vm_state : VM.t;
  current_level : int
}

type 'a effect =
| Commit : state * hash * level -> state effect
| Submit : state * VM.action list -> state effect

module Effect = struct
  include Preface.Make.Freer_monad.Over (struct
    type 'a t = 'a effect
  end)

  let commit state hash level = perform @@ Commit (state, hash, level)
  let submit state actions = perform @@ Submit (state, actions)
end

let handler : type b. (b -> 'a) -> b Effect.f -> 'a =
  fun k -> function
  | Submit (state, actions) ->
    let vm_state = List.fold_left VM.apply state.vm_state actions in
    do_tezos_interop (fun result ->
      let state handle_result state result in k state)
    k {state with vm_state}
  | Commit (state, _hash, _level) -> 
    k state
 