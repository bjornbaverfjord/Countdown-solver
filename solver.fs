// http://www.cs.nott.ac.uk/~pszgmh/countdown.pdf
// DOI: 10.1017/S0956796801004300 
// Solver for the countdown numbers game
// Translated from Haskell to F#

type Op = Add | Sub | Mul | Div
type expr = Val of int | App of Op * expr * expr

let rec split = function
  | [] -> [([], [])]
  | x :: xs -> ([], x::xs) :: [ for (ls, rs) in split xs -> (x::ls, rs) ]

let ne = function
  | (_::_, _::_) -> true
  | _            -> false

let nesplit = split >> List.filter ne

let ops = [Add; Sub; Mul; Div] 

let valid op x y = 
  match op with
  | Add -> true
  | Sub -> x > y
  | Mul -> true
  | Div -> x % y = 0 

let valid' op x y =
  match op with 
  | Add -> x <= y
  | Sub -> x > y
  | Mul -> x <> 1 && y <> 1 && x <= y 
  | Div -> y <> 1 && x % y = 0

let apply op x y =
  match op with 
  | Add -> x + y
  | Sub -> x - y
  | Mul -> x * y
  | Div -> x / y

let combine l r = [ for o in ops -> App (o,l,r) ]

let combine' (l,x) (r,y) = [ for o in ops do if valid o x y then yield (App (o,l,r), apply o x y) ] 

let combine'' (l,x) (r,y) = [ for o in ops do if valid' o x y then yield (App (o,l,r), apply o x y) ] 

let rec exprs = function
  | [] -> []
  | [n] -> [Val n]
  | ns  -> [ for ls,rs in nesplit ns do
               for l in exprs ls do
                 for r in exprs rs do
                   for e in combine l r -> e ]

let rec eval = function
  | Val n       -> [ if n > 0 then yield n ]
  | App (o,l,r) -> [ for x in eval l do
                       for y in eval r do
                         if valid o x y then yield apply o x y ] 

let rec subs = function
    | [] -> [[]]
    | x::xs -> [ for ys in subs xs do
                    yield! [ys; x::ys] ]

let rec interleave x = function
    | [] -> [[x]]
    | y::ys -> 
        [ yield x::y::ys
          for zs in interleave x ys do
            yield! [y::zs] ]

let rec perms = function
    | [] -> [[]]
    | x::xs -> List.concat (List.map (interleave x) (perms xs))

let subbags xs =
  [ for ys in subs xs do
      for zs in perms ys -> zs ]

let rec results = function
  | []  -> []
  | [n] -> [ if n > 0 then yield (Val n, n) ]
  | ns  -> [ for (ls,rs) in nesplit ns do 
               for lx in results ls do 
                 for ry in results rs do 
                   for res in combine' lx ry -> res ]

let rec results' = function
  | []  -> []
  | [n] -> [ if n > 0 then yield (Val n, n) ]
  | ns  -> [ for (ls,rs) in nesplit ns do 
               for lx in results' ls do 
                 for ry in results' rs do 
                   for res in combine'' lx ry -> res ]

let solutions ns n =
  [ for ns' in subbags ns do
      for e in exprs ns' do
        if eval e = [n] then yield e ]

let solutions' ns n =
  [ for ns' in subbags ns do 
      for (e,m) in results ns' do
        if n = m then yield e ]

let solutions'' ns n =
  [ for ns' in subbags ns do 
      for (e,m) in results' ns' do
        if n = m then yield e ]

let s = solutions [1;3;7;10;25;50]  765
printfn "Brute force: %A solutions" (s.Length)

let s' = solutions' [1;3;7;10;25;50]  765
printfn "Fused generation and evaluation: %A solutions" (s'.Length)

let s'' = solutions'' [1;3;7;10;25;50]  765
printfn "Fused with reduced search space: %A solutions" (s''.Length)
