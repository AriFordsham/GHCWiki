{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Hs.Extension where

import Data.Data hiding ( Fixity )
import Outputable
import SrcLoc (SrcSpan,Located,GenLocated(..), noLoc, getLoc, unLoc)
import ApiAnnotation
import SrcLoc


import Data.Kind
import Data.Monoid

-- | The API Annotations are now kept in the HsSyn AST for the GhcPs
--   phase. We do not always have API Annotations though, only for
--   parsed code. This type captures that, and allows the
--   representation decision to be easily revisited as it evolves.
data ApiAnn = ApiAnn [AddApiAnn] -- ^ Annotations added by the Parser
            | ApiAnnNotUsed      -- ^ No Annotation for generated code,
                                 -- e.g. from TH, deriving, etc.
        deriving (Data, Show, Eq)

noAnn :: ApiAnn
noAnn = ApiAnnNotUsed

-- | Used as a data type index for the hsSyn AST
data GhcPass (c :: Pass)
deriving instance Eq (GhcPass c)
deriving instance Typeable c => Data (GhcPass c)

data Pass = Parsed | Renamed | Typechecked
         deriving (Data)

-- Type synonyms as a shorthand for tagging
type GhcPs   = GhcPass 'Parsed      -- Old 'RdrName' type param
type GhcRn   = GhcPass 'Renamed     -- Old 'Name' type param
type GhcTc   = GhcPass 'Typechecked -- Old 'Id' type para,
type GhcTcId = GhcTc                -- Old 'TcId' type param

-- | GHC's L prefixed variants wrap their vanilla variant in this type family,
-- to add 'SrcLoc' info via 'Located'. Other passes than 'GhcPass' not
-- interested in location information can define this instance as @f p@.
type family XRec p (f :: * -> *) = r | r -> p f
type instance XRec GhcPs f = LocatedA (f GhcPs)
type instance XRec GhcRn f = Located  (f GhcRn)
type instance XRec GhcTc f = Located  (f GhcTc)

type LocatedA = GenLocated SrcSpanAnn

data SrcSpanAnn = SrcSpanAnn { ann :: ApiAnn, loc :: SrcSpan }
data AddApiAnn = AddApiAnn AnnKeywordId SrcSpan deriving (Data,Show,Eq)

data RdrName p = RdrName String
data Name    p = Name String
data Id      p = Id String

-- | Maps the "normal" id type for a given pass
type family IdPP p :: * -> *
type instance IdPP GhcPs   = RdrName
type instance IdPP GhcRn   = Name
type instance IdPP GhcTc   = Id
type instance IdPP GhcTcId = Id

type IdP p = IdPP p p

-----------------------

data Exp p = Exp1 (LIdP p)
           | Exp2 (LIdP p) (LIdP p)

type LIdP p = XRec p (IdPP p)
type LExp p = XRec p Exp

doFooR :: LIdP GhcRn -> LExp GhcRn
doFooR ln = noLoc (Exp1 ln)

doFooP :: IdP GhcPs -> LExp GhcPs
doFooP n = noLocP (Exp1 (noLocP n))

noLocP :: a GhcPs -> XRec GhcPs a
noLocP e = L (SrcSpanAnn ApiAnnNotUsed noSrcSpan) e

-- --------------------------

-- This is messy
baz :: XRec (GhcPass p) Exp ~ GenLocated l (Exp (GhcPass p))
    => LExp (GhcPass p) -> Int
baz (L _ (Exp1 ln)) = 4

-- ----------------------
