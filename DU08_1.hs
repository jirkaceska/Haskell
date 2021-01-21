import Data.List
-- Kostra 8. domácí úlohy IB015
-- Řešení vkládejte do odpovědníku bez definice datových typů Term a Formula a
-- typové třídy Pretty.

-- Nezapomeňte do odpovědníku vložit i všechny importy!
-- 8< -------------------------------------------------------------------------

data Term = Var String                      -- proměnná
          | Fun String [Term]               -- f(t1, .., tn) nebo f
          deriving (Eq, Show, Read)

data Formula = Pred String [Term]           -- P(t1, .., tn) nebo P
             | Equal Term Term              -- t1 = t2
             | Not Formula                  -- ¬φ
             | And Formula Formula          -- φ ∧ ψ
             | Or Formula Formula           -- φ ∨ ψ
             | Implies Formula Formula      -- φ ⇒ ψ
             | Exists String Formula        -- ∃x φ
             | Forall String Formula        -- ∀x φ
             deriving (Eq, Show, Read)

class Pretty a where
    pprint :: a -> String

-- Do odpovědníku NEVKLÁDEJTE kód výše!
-- 8< -------------------------------------------------------------------------
type Quantifier = String

-- Převod do negation normal form.
-- V NNF smí být negace aplikovaná pouze na predikáty a porovnání termů, nikoli
-- na složitější formule. Navíc se v ní nesmí vyskytovat implikace.
nnf :: Formula -> Formula
nnf (Not (Not f)) = nnf f
nnf (Not (And f1 f2)) = Or (nnf (Not f1)) (nnf (Not f2))
nnf (Not (Or f1 f2)) = And (nnf (Not f1)) (nnf (Not f2))
nnf (Not (Implies f1 f2)) = And (nnf f1) (nnf (Not f2))
nnf (Not (Exists var f)) = nnf (Forall var (nnf (Not f)))
nnf (Not (Forall var f)) = nnf (Exists var (nnf (Not f)))
nnf (And f1 f2) = And (nnf f1) (nnf f2)
nnf (Or f1 f2) = Or (nnf f1) (nnf f2)
nnf (Implies f1 f2) = Or (nnf (Not f1)) (nnf f2)
nnf (Exists var f) = Exists var (nnf f)
nnf (Forall var f) = Forall var (nnf f)
nnf f = f


-- Smysluplnost formule.
-- Zkontroluje, zda jsou všechny použité proměnné kvantifikované a zda
-- nekolidují jejich jména.
validate :: Formula -> Bool
validate = validateQuantified []

validateTerm :: [Quantifier] -> Term -> Bool
validateTerm _ (Fun _ []) = True
validateTerm qs (Fun _ terms) = all (validateTerm qs) terms
validateTerm qs (Var name) = any (== name) qs

validateQuantified :: [Quantifier] -> Formula -> Bool
validateQuantified qs (Not f) = validateQuantified qs f

validateQuantified qs (And f1 f2) = validateQuantified qs f1 && validateQuantified qs f2
validateQuantified qs (Or f1 f2) = validateQuantified qs f1 && validateQuantified qs f2
validateQuantified qs (Implies f1 f2) = validateQuantified qs f1 && validateQuantified qs f2

validateQuantified qs (Exists quantifier f) = all (/= quantifier) qs && validateQuantified (quantifier:qs) f
validateQuantified qs (Forall quantifier f) = all (/= quantifier) qs && validateQuantified (quantifier:qs) f
-- Otherwise check quantified terms
validateQuantified qs (Equal t1 t2) = validateTerm qs t1 && validateTerm qs t2
validateQuantified qs (Pred _ terms) = all (validateTerm qs) terms

-- Snadno čitelný výpis termů a formulí

prettyArgs :: [Term] -> String
prettyArgs = ('(' :) .
             tail . 
             (foldr (\term acc -> ", " ++ pprint term  ++ acc) " )")

closeInSimpleBrackets :: String -> String
closeInSimpleBrackets = ("( " ++) . (++ " )")

injectLogicOperation :: Pretty a => String -> [a] -> String
injectLogicOperation op = foldr1 (\term acc -> term ++ " " ++ op ++ " " ++ acc) .
                          map  pprint



instance Pretty Term where
    pprint (Var name) = name
    pprint (Fun name []) = name
    pprint (Fun name args) = name ++ prettyArgs args 

instance Pretty Formula where
    pprint (Pred name []) = name
    pprint (Pred name args) = name ++ prettyArgs args
    pprint (Equal t1 t2) = injectLogicOperation "=" [t1, t2]
    pprint (Not f) = '¬' : pprint f

    pprint (And (And f1 f2) (And f3 f4)) = injectLogicOperation "∧" [f1, f2, f3, f4]
    pprint (And (And f1 f2) f3) = closeInSimpleBrackets (injectLogicOperation "∧" [f1, f2, f3])
    pprint (And f1 (And f2 f3)) = closeInSimpleBrackets (injectLogicOperation "∧" [f1, f2, f3])
    pprint (And f1 f2) = closeInSimpleBrackets (injectLogicOperation "∧" [f1, f2])
    
    pprint (Or (Or f1 f2) (Or f3 f4)) = injectLogicOperation "∨" [f1, f2, f3, f4]
    pprint (Or (Or f1 f2) f3) = closeInSimpleBrackets (injectLogicOperation "∨" [f1, f2, f3])
    pprint (Or f1 (Or f2 f3)) = closeInSimpleBrackets (injectLogicOperation "∨" [f1, f2, f3])
    pprint (Or f1 f2) = closeInSimpleBrackets (injectLogicOperation "∨" [f1, f2] )
    
    pprint (Implies f1 f2) = closeInSimpleBrackets (injectLogicOperation "⇒" [f1, f2])
    pprint (Exists var f) = '∃' : var ++ " " ++ pprint f
    pprint (Forall var f) = '∀' : var ++ " " ++ pprint f   
    


-- 8< -------------------------------------------------------------------------
-- Jednoduché testovací formule


formula1 :: Formula
formula1 = Exists "z" (Forall "x" (Exists "y" (Equal (Var "x") (Var "y"))))

formula2 :: Formula
formula2 = Not (Implies (Equal (Var "A") (Fun "f" [Var "A"]))
                        (Equal (Var "A") (Fun "g" [Fun "c" []])))

formula3 :: Formula
formula3 = And (Pred "P" [Fun "3" []])
               (And (Pred "Q" [Var "X"])
                    (Equal (Var "X") (Fun "3" [])))
                    
formulaMy :: Formula
formulaMy = And (Pred "P" [])
                (And (
                        And (
                                And (Pred "Q" [])
                                    (Pred "T" [])
                            )
                            (
                                And (Pred "R" [])
                                    (Pred "U" [])
                            )
                     )
                     (Pred "S" [])
                )

formula4 :: Formula
formula4 = Forall "x" (Pred "P" [Var "x"] `Implies`
                          (Pred "P" [] `Implies`
                              Pred "P" [Fun "x" [], Fun "x" [Var "x"]]))

formula5 :: Formula
formula5 = Not (Forall "A" (Exists "B"
                    (Not ((Pred "P" [Var "A", Fun "5555" []] `Implies`
                        Not (Pred "Q" [Fun "5555" [], Var "B"])) `And`
                            (Not (Pred "R" []) `Or` Not (Pred "S" [Var "B"]))))))
