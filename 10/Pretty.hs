module Pretty where
 
import Grammar (Type(..), Term(..), NamedTerm(..))

import Text.PrettyPrint (Doc, (<>), (<+>), hsep, parens, text)

term :: Term -> Doc
term TTrue = text "true"
term TFalse = text "false"
term (TIf t1 t2 t3) = hsep [ text "if"
                                , term t1
                                , text "then"
                                , term t2
                                , text "else"
                                , term t3
                                ]
term (TVar i) = text $ show i
term (TAbs ty t) = text "λ" <+> typ ty <> text "." <+> term t
term (TApp t1 t2) = parens (term t1 <+> term t2)

typ :: Type -> Doc
typ TyBool = text "Bool"
typ (TyArr ty1 ty2) = left <+> text "→" <+> typ ty2
  where left = case ty1 of
          TyArr{} -> parens . typ $ ty1
          _       -> typ ty1

namedTerm :: NamedTerm -> Doc
namedTerm NTrue = text "true"
namedTerm NFalse = text "false"
namedTerm (NIf t1 t2 t3) = hsep [ text "if"
                                , namedTerm t1
                                , text "then"
                                , namedTerm t2
                                , text "else"
                                , namedTerm t3
                                ]
namedTerm (NVar s)     = text s
namedTerm (NAbs ty s t)   = hsep [ text $ "λ" ++ s ++ ":"
                                 , typ ty
                                 , text "."
                                 , (namedTerm t)]

namedTerm (NApp t1 t2) = prettyRand <+> prettyRator
  where prettyRand = case t1 of
          NAbs{} -> wrapped t1
          _      -> namedTerm t1
        prettyRator = case t2 of
          NVar{} -> namedTerm t2
          _      -> wrapped t2
        wrapped = parens . namedTerm
