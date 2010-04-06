module FunLang.Intermediate.Conditional where

import FunLang.Intermediate.Desugared

conditional ((cond, cons):rest) alt org =
    Conditional cond cons (conditional rest alt org) org

conditional [] alt _ = alt

