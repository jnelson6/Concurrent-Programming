-module(realq6).
-compile(export_all).
%NOT MINE

ic({empty}) ->
  true;
ic({node,I,L,R}) ->
  ich({node, I, L, R}, 0, tSize({node,I, L,R})).

ich({empty},_,_) ->
  true;
ich({node, I, L, R}, IND, NCount) when IND >= NCount ->
  false;
ich({node, I, L, R}, IND, NCount) ->
  ich(L, 2*IND+1, NCount) and ich(R, 2*IND+2, NCount).




tSize({empty}) ->
  0;
tSize({node, I, L, R}) ->
  1 + tSize(L) + tSize(R).