open Sim

exception Invalid

let show_usage () =
    Printf.printf "Usage: %s command arguments\n" Sys.argv.(0);
    print_string "where command is one of\n";
    print_string "   gcc file.gcc : runs file.gcc and show trace ouput"

let _ =
    Printexc.record_backtrace true;
    try
        if Array.length Sys.argv = 1
        then raise Invalid;
        match Sys.argv.(1) with
        | "gcc" -> begin
            if Array.length Sys.argv <> 3
            then raise Invalid;
            let gcc_fn = Sys.argv.(2) in
            Printf.printf "Loading file %s\n" gcc_fn;
            let gcc = Gcc.read_gcc_from_file gcc_fn in
            let mac = Gccsim.dummy_machine () in
            Gccsim.init_machine mac gcc;
            let end_cycle = Gccsim.run ~verbose:true true mac in
            Printf.printf "Execution ended after %d cycles.\n" end_cycle
        end;
        | "sim" -> begin
            if Array.length Sys.argv < 5
            then raise Invalid;
            load_map Sys.argv.(2);
            let lambdaman_code = Gcc.read_gcc_from_file Sys.argv.(3) in
            let ghosts_code = Array.map Ghc.read_ghc_from_file
                (Array.sub Sys.argv 4 (Array.length Sys.argv - 4)) in
            try
                let _ = run_sim map ghosts_code lambdaman_code in
                ()
            with e -> ()
                (*
                Printexc.print_backtrace stdout;
                raise e
                *)
        end
        | "compile" -> begin
            let gcc, src = Compiler.compile Sys.argv.(2) in
            for i = 0 to Array.length gcc - 1 do
                print_string (Gcc.pp_instr src gcc.(i));
                print_newline ()
            done
        end
        | _ -> raise Invalid
    with Invalid -> show_usage ()
