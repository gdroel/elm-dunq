module Main exposing (..)

import Html

type alias Binding = 
  { name : String, 
    value : Value }

type alias Env = List Binding

type ExprC = 
  NumC Float | 
  StringC String |
  AppC ExprC List ExprC

type Value = 
  NumV Float |
  StringV String |
  ErrorV String |
  PrimVPlus

----------------------------
-- Interpreting functions --
----------------------------

-- Interprets an AST --
interp : ExprC -> List String -> Value
interp expr env =
  case expr of 
    NumC n ->
      (NumV n)
    StringC s -> 
      (StringV s)
    AppC appc_expr args ->
      (ErrorV "Not implemented yet")
      --case (interp appc_expr env) of
      --  PrimVPlus ->

          -- 

-- Inteprets a plus, can take any amount of args
interp_plus : List Value -> Value
interp_plus values =
  case (List.head curr_env) of
    Nothing -> 0
    Just value ->
      case value of
        NumV n ->
          (NumV n + (interp_plus (List.drop 1 curr_env)).n)

---------------------------
-- Environment functions --
---------------------------

-- Creates a new environment -- 
env_new : List Binding 
env_new = []

-- Extends an existing environment -- 
-- "::" is the elm version of cons --
env_extend : Env -> Binding -> Env
env_extend curr_env binding = 
  binding :: curr_env

-- Looks up value in environment --
env_lookup : Env -> String -> Value
env_lookup curr_env name =
  case (List.head curr_env) of
    Nothing ->
      ErrorV "Value not found in environment"
    Just binding ->
      if binding.name == name then
        binding.value
      else
        env_lookup (List.drop 1 curr_env) name

-- Checks if two things are equal
check_equal test expected =
  if test == expected then
    "Success\n"
  else
    "Failure: " ++ (Debug.toString test) ++ " != " ++ (Debug.toString expected) 

test_env = (env_extend (env_extend env_new (Binding "first" (NumV 1))) (Binding "second" (NumV 2))) 
    
-- Add all tests here 
test_list = [
  -- Interpretation Tests --
  (check_equal (interp (NumC 4) []) (NumV 4)),
  (check_equal (interp (StringC "hi") []) (StringV "hi")),

  --- Environmnent Tests -- 
  (check_equal env_new []),
  (check_equal (env_extend env_new (Binding "first" (NumV 4))) [(Binding "first" (NumV 4))]),
  (check_equal (env_lookup test_env "second") (NumV 2)),
  (check_equal (env_lookup [] "dne") (ErrorV "Value not found in environment"))
  ]
 

main = Html.text (Debug.toString test_list)
  