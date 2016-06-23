module Components.Term.Note.Gen where

import Common.Term.Gen

import Components.Term.Note.Data

genTermInput :: WithNoteTerm n ty tm
             => GenTermInput ty tm
genTermInput = mempty
