(****************************************************************************)
(* This Source Code Form is subject to the terms of the                     *)
(* Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed *)
(* with this file, You can obtain one at http://mozilla.org/MPL/2.0/.       *)
(****************************************************************************)

open Core

(** A module for measuring the execution time of arbitrary code blocks. *)
module TimeProfiler = struct
  (** A single time measurement record. *)
  type measurement =
    { start_time : float (** The high-resolution start time in seconds. *)
    ; end_time : float option
      (** The high-resolution end time in seconds, if completed. *)
    ; label : string (** A unique identifier for the measurement. *)
    ; category : string (** A category for grouping measurements. *)
    }

  (** The state of the time profiler. *)
  type t =
    { mutable measurements : measurement list (** A list of completed measurements. *)
    ; mutable active_measurements : (string * float) list
      (** A list of currently active (started but not ended) measurements. *)
    }

  (** Creates a new, empty time profiler instance. *)
  let create () = { measurements = []; active_measurements = [] }

  (** Starts a new time measurement.
      Records the current time and associates it with a label.
      @param profiler The time profiler instance.
      @param label A unique identifier for the measurement.
      @param _category A category for the measurement (currently unused). *)
  let start_measurement profiler label _category =
    let start_time =
      Time_now.nanoseconds_since_unix_epoch () |> Int63.to_float |> fun x -> x /. 1e9
    in
      profiler.active_measurements <- (label, start_time) :: profiler.active_measurements
  ;;

  (** Ends a time measurement.
      Finds the corresponding start time by label, calculates the duration,
      and records the completed measurement.
      @param profiler The time profiler instance.
      @param label The identifier of the measurement to end.
      @return The duration of the measurement in seconds, or [None] if the label was not found. *)
  let end_measurement profiler label =
    match
      List.find profiler.active_measurements ~f:(fun (l, _) -> String.equal l label)
    with
    | Some (_, start_time) ->
      let end_time =
        Time_now.nanoseconds_since_unix_epoch () |> Int63.to_float |> fun x -> x /. 1e9
      in
      let measurement =
        { start_time; end_time = Some end_time; label; category = "default" }
      in
        profiler.measurements <- measurement :: profiler.measurements;
        profiler.active_measurements
        <- List.filter profiler.active_measurements ~f:(fun (l, _) ->
             not (String.equal l label));
        Some (end_time -. start_time)
    | None ->
      None
  ;;

  (** Returns all completed measurements.
      @param profiler The time profiler instance. *)
  let get_measurements profiler = profiler.measurements

  (** Aggregated statistics for a set of measurements. *)
  type stats =
    { total_time : float (** The sum of all measurement durations. *)
    ; count : int (** The total number of measurements. *)
    ; average_time : float (** The average duration of a measurement. *)
    }

  (** Calculates and returns the aggregated statistics for all completed measurements.
      @param profiler The time profiler instance. *)
  let get_stats profiler =
    let measurements = profiler.measurements in
    let total_time =
      List.fold measurements ~init:0.0 ~f:(fun acc m ->
        match m.end_time with
        | Some end_t ->
          acc +. (end_t -. m.start_time)
        | None ->
          acc)
    in
    let count = List.length measurements in
      { total_time
      ; count
      ; average_time =
          (if count > 0 then
             total_time /. float_of_int count
           else
             0.0)
      }
  ;;
end

(** A module for profiling function calls to gather statistics like call count and execution time. *)
module FunctionProfiler = struct
  (** Statistical information for a single profiled function. *)
  type call_info =
    { name : string (** The name of the function. *)
    ; call_count : int (** The number of times the function has been called. *)
    ; total_time : float (** The total cumulative time spent in the function. *)
    ; min_time : float (** The shortest execution time for a single call. *)
    ; max_time : float (** The longest execution time for a single call. *)
    ; last_called : float (** The timestamp of the last call. *)
    }

  (** The state of the function profiler, using a hash table to store call information. *)
  type t = { functions : (string, call_info) Hashtbl.t }

  (** Creates a new, empty function profiler. *)
  let create () = { functions = Hashtbl.create (module String) }

  (** Wraps a function call to profile its execution.
      It measures the execution time of the given function [f] and updates the
      statistics associated with [name].
      @param profiler The function profiler instance.
      @param name The name to associate with the profiled function.
      @param f The function to execute and profile.
      @return The result of the function [f]. *)
  let profile_call profiler name f =
    let start_time =
      Time_now.nanoseconds_since_unix_epoch () |> Int63.to_float |> fun x -> x /. 1e9
    in
    let result = f () in
    let end_time =
      Time_now.nanoseconds_since_unix_epoch () |> Int63.to_float |> fun x -> x /. 1e9
    in
    let elapsed = end_time -. start_time in
    let call_info =
      match Hashtbl.find profiler.functions name with
      | Some info ->
        { name = info.name
        ; call_count = info.call_count + 1
        ; total_time = info.total_time +. elapsed
        ; min_time = Float.min info.min_time elapsed
        ; max_time = Float.max info.max_time elapsed
        ; last_called = end_time
        }
      | None ->
        { name
        ; call_count = 1
        ; total_time = elapsed
        ; min_time = elapsed
        ; max_time = elapsed
        ; last_called = end_time
        }
    in
      Hashtbl.set profiler.functions ~key:name ~data:call_info;
      result
  ;;

  (** Retrieves the statistics for a specific function.
      @param profiler The function profiler instance.
      @param name The name of the function.
      @return An [option] containing the call info if found. *)
  let get_function_stats profiler name = Hashtbl.find profiler.functions name

  (** Returns a list of all collected function statistics.
      @param profiler The function profiler instance. *)
  let get_all_stats profiler = Hashtbl.data profiler.functions

  (** Returns the top [n] functions sorted by total execution time in descending order.
      @param profiler The function profiler instance.
      @param n The number of top functions to return. *)
  let get_top_functions profiler n =
    let all_stats = get_all_stats profiler in
      List.sort all_stats ~compare:(fun a b -> Float.compare b.total_time a.total_time)
      |> Fn.flip List.take n
  ;;
end

(** A module for taking snapshots of memory and garbage collector statistics. *)
module MemoryProfiler = struct
  (** A snapshot of GC statistics at a point in time. See {!Gc.stat}. *)
  type snapshot =
    { timestamp : float (** The high-resolution timestamp of the snapshot. *)
    ; heap_words : int (** Total size of the major heap, in words. *)
    ; live_words : int (** Number of live words in the major heap. *)
    ; free_words : int (** Number of free words in the major heap. *)
    ; stack_size : int (** Size of the stack, in words. *)
    ; minor_collections : int (** Number of minor collections since program start. *)
    ; major_collections : int (** Number of major collections since program start. *)
    }

  (** The state of the memory profiler. *)
  type t =
    { mutable snapshots : snapshot list
      (** A list of memory snapshots, with the most recent at the head. *)
    ; mutable last_gc_stats : Gc.stat (** The last recorded GC statistics. *)
    }

  (** Creates a new, empty memory profiler. *)
  let create () = { snapshots = []; last_gc_stats = Gc.stat () }

  (** Captures the current GC statistics and records a snapshot. *)
  let take_snapshot profiler =
    let now =
      Time_now.nanoseconds_since_unix_epoch () |> Int63.to_float |> fun x -> x /. 1e9
    in
    let current_stats = Gc.stat () in
    let snapshot =
      { timestamp = now
      ; heap_words = current_stats.heap_words
      ; live_words = current_stats.live_words
      ; free_words = current_stats.free_words
      ; stack_size = current_stats.stack_size
      ; minor_collections = current_stats.minor_collections
      ; major_collections = current_stats.major_collections
      }
    in
      profiler.snapshots <- snapshot :: profiler.snapshots;
      snapshot
  ;;

  (** Statistics about memory growth over a period. *)
  type growth_stats =
    { heap_growth : int (** The change in heap words. *)
    ; live_growth : int (** The change in live words. *)
    ; collections_since_start : int
      (** The number of minor collections during the period. *)
    }

  (** Calculates memory growth between the first and the last snapshot.
      @param profiler The memory profiler instance. *)
  let get_memory_growth profiler =
    match profiler.snapshots with
    | []
    | [ _ ] ->
      (* Not enough data for a growth calculation *)
      { heap_growth = 0; live_growth = 0; collections_since_start = 0 }
    | latest_snapshot :: _ ->
      (* The list is stored with the most recent snapshot at the head.
         The oldest snapshot is at the end of the list. *)
      let first_snapshot = List.last_exn profiler.snapshots in
        { heap_growth = latest_snapshot.heap_words - first_snapshot.heap_words
        ; live_growth = latest_snapshot.live_words - first_snapshot.live_words
        ; collections_since_start =
            latest_snapshot.minor_collections - first_snapshot.minor_collections
        }
  ;;

  (** Returns the most recent memory snapshot, if one exists.
      @param profiler The memory profiler instance. *)
  let get_latest_snapshot profiler =
    match profiler.snapshots with
    | [] ->
      None
    | hd :: _ ->
      Some hd
  ;;
end

(** A summary of all performance metrics collected by the profiler. *)
type performance_stats =
  { time_stats : TimeProfiler.stats (** Aggregated statistics from the time profiler. *)
  ; memory_growth : MemoryProfiler.growth_stats (** Memory growth statistics. *)
  ; top_functions : FunctionProfiler.call_info list
    (** A list of the most time-consuming functions. *)
  ; total_function_calls : int (** The total number of calls to all profiled functions. *)
  ; average_call_time : float
    (** The average time per function call across all profiled functions. *)
  }

(** The main performance monitor, integrating time, function, and memory profilers. *)
type t =
  { time_profiler : TimeProfiler.t (** The time profiler instance. *)
  ; function_profiler : FunctionProfiler.t (** The function profiler instance. *)
  ; memory_profiler : MemoryProfiler.t (** The memory profiler instance. *)
  ; mutable enabled : bool (** A flag to enable or disable profiling globally. *)
  }

let create () =
  { time_profiler = TimeProfiler.create ()
  ; function_profiler = FunctionProfiler.create ()
  ; memory_profiler = MemoryProfiler.create ()
  ; enabled = true
  }
;;

(** Enables the profiler. Profiling calls will be active. *)
let enable profiler = profiler.enabled <- true

(** Disables the profiler. Profiling calls will be inexpensive no-ops. *)
let disable profiler = profiler.enabled <- false

(** Checks if the profiler is currently enabled. *)
let is_enabled profiler = profiler.enabled

(** Starts a time measurement if the profiler is enabled. *)
let start_measurement profiler label category =
  if profiler.enabled then
    TimeProfiler.start_measurement profiler.time_profiler label category
;;

(** Ends a time measurement if the profiler is enabled. *)
let end_measurement profiler label =
  if profiler.enabled then
    TimeProfiler.end_measurement profiler.time_profiler label
  else
    None
;;

(** Profiles a function call if the profiler is enabled.
    If disabled, it simply executes the function. *)
let profile_function profiler name f =
  if profiler.enabled then
    FunctionProfiler.profile_call profiler.function_profiler name f
  else
    f ()
;;

(** Takes a memory snapshot if the profiler is enabled. *)
let take_memory_snapshot profiler =
  if profiler.enabled then
    Some (MemoryProfiler.take_snapshot profiler.memory_profiler)
  else
    None
;;

(** Gathers and returns a comprehensive report of all collected performance statistics. *)
let get_performance_stats profiler =
  let time_stats = TimeProfiler.get_stats profiler.time_profiler in
  let memory_growth = MemoryProfiler.get_memory_growth profiler.memory_profiler in
  let all_functions = FunctionProfiler.get_all_stats profiler.function_profiler in
  let top_functions = FunctionProfiler.get_top_functions profiler.function_profiler 10 in
  let total_calls =
    List.sum (module Int) all_functions ~f:(fun info -> info.call_count)
  in
  let total_time =
    List.sum (module Float) all_functions ~f:(fun info -> info.total_time)
  in
  let average_call_time =
    if total_calls > 0 then
      total_time /. float_of_int total_calls
    else
      0.0
  in
    { time_stats
    ; memory_growth
    ; top_functions
    ; total_function_calls = total_calls
    ; average_call_time
    }
;;

(** A convenience helper to measure the execution time of a function.
    This is a simple wrapper around [start_measurement] and [end_measurement].
    @param profiler The main profiler instance.
    @param label A label for the measurement.
    @param f The function to execute and measure. *)
let measure_time profiler label f =
  start_measurement profiler label "measurement";
  let result = f () in
    ignore (end_measurement profiler label);
    result
;;

(** A convenience alias for [profile_function]. *)
let measure_function profiler name f = profile_function profiler name f
