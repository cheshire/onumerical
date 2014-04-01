# ONumerical

OCaml library featuring:

 * Sparse Linear Expressions

    Functorized module for dealing with linear expressions over any number
    type and over any variable type (Zarith rationals, floats, integers for
    numbers and strings for variables are supported out-of-the-box):

    <pre>
        module Expression = Expression_f.Make(Str_var)(Float_number);;
        let a = Expression.of_assoc_list_and_const
            [("x", 1.0), ("y", 1.0)] 0.0;;
        let b = Expression.of_assoc_list_and_const
            [("x", -1.0), ("z", 3.0)] 5.0;;
        Expression.(to_string (a ++ b));;
        (* Output: 1 (y) + 3 (z) + 5 *)
    </pre>

 * Generic vectors, represented using arrays:

    <pre>
        module Vector = Vector_f.Make(Float_number);;
        let a = [| 0.0; 3.0; 4.0 |];;
        let b = [| 2.0; 8.0; 2.0 |];;
        Vector.(to_string (a -- b));;
        (* Output: [-2.0    -5.0    2.0] *)
    </pre>

 * Generic matrixes represented using arrays:

    <pre>
        module Matrix = Matrix_f.Make(Float_number);;
        let a = [|
            [|2.0; 3.0; 4.0|];
            [|1.0; 9.0; 1.0|];
            [|0.5; -1.0; 0.0|];
        |];;
         let b = [|
            [|2.0; 3.0; 4.0|];
            [|3.0; 6.0; 3.0|];
            [|0.5; -1.0; 3.0|];
        |];;
        Matrix.(to_string (a *** b))
        (* Output:
        | 15.0    20.0    29.0|
        | 29.5    56.0    34.0|
        |-2.0     -4.5     1.0|
        *)
    </pre>

 * Dual Simplex Solver on expressions:

    Solves linear optimization problems which are dual-feasible.

    <pre>
        module Solver = Opt_solver_f.Make(Str_var)(Float_number)
        module Expression = Solver.Expression
        let constraints = [
            ([("x1", -1.0); ("x2", -1.0); ("x3", 2.0)], 3.0);
            ([("x1", -4.0); ("x2", -2.0); ("x3", 1.0)], 4.0);
            ([("x1", 1.0); ("x2", 1.0); ("x3", -4.0)], -2.0);
        ];;
        let opt_problem = Solver.of_constraints_and_objective
            (List.map constraints ~f:(fun (coeffs, const) -> (
                Opt.LessThanZero,
                    (Expression.of_assoc_list_and_const coeffs const)
            )))
            (Opt.Maximize
                Expression.of_assoc_list_and_const
                    [("x1", -4.0); ("x2", -2.0); ("x3", -1.0)]
                    0.0
            );;
        let Opt.Solution solution = Opt.solve opt_problem;;
        solution.primal_var_assignment
        (* output: [("x1", 0.0); ("x2", 4.0, "x3", 0.5)] *)
    </pre>

### Sample Application: Balancing Chemical Equations

There is a typical problem in high-school chemistry: balance (add coefficients
to) a reaction equation.
For example we start with a simple equation

    H2S + O2 -> H2O + SO2

And we wish to obtain the balanced version

    2 H2S + 3 O2 -> 2 H2O + 2 SO2

While for the simple reactions the problem is trivial enough to be given as a
homework, the solution in the general case requires the application of a
simplex method (minimize the sum of all constraints subject to balancing atoms
on left and right hand side and subject to the strict positivity constraint).

Moreover, since we want to minimize the quantity with no negative coefficients
the problem is dual-feasible.
The solution is implemented as a sample application of `ONumerical`, and can
be launched as a standalone executable:

<pre>
    echo "H2S + O2 -> H2O + SO2" | ./chem_balancer.native
    2H2S + 3O2 -> 2H2O + 2SO2
</pre>

### Compiling and Installation

Compiling:

    $ make

Running tests:

    $ make run_test

Compiling the balancer:

    $ make chem_balancer
    $ ./chem_balancer # to run

### Dependencies

 * `Dolog`
 * `OCamlbuild` for complilation
 * `Core`
 * `Re2`
 * `OUnit` if you want to run unit tests

### NOTE

Dual Simplex uses a very inefficient dense representation and is not optimized
for performance.
So are the linear algebra primitives, as they re-create the data-structure
after each operation.
