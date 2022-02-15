module Juvix.Context.InfoNames where

import Juvix.Library

signature :: IsString p => p
signature = "type"

precedence :: IsString p => p
precedence = "precedence"

usage :: IsString p => p
usage = "usage"
