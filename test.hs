import Hw06

successor = (Lam "n" (Lam "s" (Lam "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))))))

zero = (Lam "s" (Lam "z" (Var "z")))

s = Map.empty

Map.insert "zero" zero 
Map.insert "succ" successor

