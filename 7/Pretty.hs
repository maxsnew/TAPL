module Pretty where

import Lambda

import Text.PrettyPrint

term :: Term -> Doc
term (Var i) = text $ show i
term (Abs t) = text "λ." <+> term t
term (App t1 t2) = parens (term t1 <+> term t2)

namedTerm :: NamedTerm -> Doc
namedTerm (NVar s)     = text s
namedTerm (NAbs s t)   = (text dec) <+> (namedTerm t)
  where dec = "λ" ++ s ++ "."
namedTerm (NApp t1 t2) = prettyRand <+> prettyRator
  where prettyRand = case t1 of
          NAbs{} -> wrapped t1
          _      -> namedTerm t1
        prettyRator = case t2 of
          NVar{} -> namedTerm t2
          _      -> wrapped t2
        wrapped = parens . namedTerm

