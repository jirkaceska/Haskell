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


-- Převod do negation normal form.
-- V NNF smí být negace aplikovaná pouze na predikáty a porovnání termů, nikoli
-- na složitější formule. Navíc se v ní nesmí vyskytovat implikace.
nnf :: Formula -> Formula
nnf = undefined

-- Smysluplnost formule.
-- Zkontroluje, zda jsou všechny použité proměnné kvantifikované a zda
-- nekolidují jejich jména.
validate :: Formula -> Bool
validate = undefined

-- Snadno čitelný výpis termů a formulí
instance Pretty Term where
    pprint = undefined

instance Pretty Formula where
    pprint = undefined


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

formula4 :: Formula
formula4 = Forall "x" (Pred "P" [Var "x"] `Implies`
                          (Pred "P" [] `Implies`
                              Pred "P" [Fun "x" [], Fun "x" [Var "x"]]))

formula5 :: Formula
formula5 = Not (Forall "A" (Exists "B"
                    (Not ((Pred "P" [Var "A", Fun "5555" []] `Implies`
                        Not (Pred "Q" [Fun "5555" [], Var "B"])) `And`
                            (Not (Pred "R" []) `Or` Not (Pred "S" [Var "B"]))))))
