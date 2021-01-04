(*Exercise 8*)
(*traced_set_intersection show_int [1; 2; 3; 4; 5; 6; 7 ;8 ;9 ;10] [5; 6; 7; 8] ;;
set_intersection [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] [5; 6; 7; 8] ->
  visit [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] ->
  visit [2; 3; 4; 5; 6; 7; 8; 9; 10] ->
  visit [3; 4; 5; 6; 7; 8; 9; 10] ->
  visit [4; 5; 6; 7; 8; 9; 10] ->
  visit [5; 6; 7; 8; 9; 10] ->
    visit [6; 7; 8; 9; 10] ->
      visit [7; 8; 9; 10] ->
        visit [8; 9; 10] ->
          visit [9; 10] ->
          visit [10] ->
          visit [] ->
          visit [] <- []
        visit [8; 9; 10] <- [8]
      visit [7; 8; 9; 10] <- [7; 8]
    visit [6; 7; 8; 9; 10] <- [6; 7; 8]
  visit [5; 6; 7; 8; 9; 10] <- [5; 6; 7; 8]
set_intersection [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] [5; 6; 7; 8] <- [5; 6; 7; 8]
- : int list = [5; 6; 7; 8]*)

(*5 tail, 3 non-tail, 3 tail, series of returns*)
(*5 returns 4 elements and of course the null set*)