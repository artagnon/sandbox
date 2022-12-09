# Student accommodation

A cohort of N graduate students (labelled with distinct integers) need to be
accommodated in a building having N rooms (also labelled with distinct
integers). The students like to procrastinate by visiting their friends’
offices during the day. Each student has a set of friends amongst the others,
each of whom they like to visit once a day. (Since students need exercise, they
will still visit their friend even if the friend has already visited them.) All
students are equally lazy, so the cost of a visit is defined to be the shortest
distance between the two students’ offices.

Write a function (in C++, Python 3, or Rust) that takes as input

- a representation of the friendship relations between the students, and
- a table of distances between offices

and outputs a mapping of rooms to students such that the total cost of each
day’s visits is as low as possible.

Note:

- The friendship relation between graduate students may be asymmetric.
- The distance function is symmetric.

Notes:

* Your solution need not be optimal. Cheap approximations are good.
* You should consider how the time and space requirements scale with N. Try to
  avoid exponential scaling.
* Please include explanations of the principles applied in your solution.

## C++ solution

We have provided a header file `student-accommodation.hpp` and a "dummy"
implementation file `student-accommodation.cpp`. You should write your own
implementation in the latter file.

Please include any important build instructions with your solution. It should be
possible to build with one or other of gcc or clang.

## Python 3 solution

We have provided a "dummy" implementation file `student-accommodation.py`. You
should write your own implementation in this file.

Please include any relevant information, including a list of any packages that
you use outside the standard library.


## Rust solution

We have provided a "dummy" implementation file `student-accommodation.rs`. You
should write your own implementation in this file.

Please include any relevant information for running the solution.

Ideally submit a crate including a `Cargo.toml` from which we can import the
implementation of the solution template.
