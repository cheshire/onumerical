open OUnit2

let () =
    Log.color_on ();
    Log.set_log_level Log.DEBUG;

    run_test_tt_main ("all" >::: [
        Test_chem_parser.tests;
        Test_dual_simplex_solver_f.tests;
        Test_opt_solver_f.tests;
    ])
