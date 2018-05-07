open OUnit2

(* (>:::) : string -> test list -> test *)

let suite = "test" >::: [
      "test_pp" >::: Test_pp.suite;
      "test_typing" >::: Test_typing.suite;
    ]

let () = run_test_tt_main suite
