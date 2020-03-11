module Main exposing (..)

import Html

type alias Binding = 
  { name : String, 
    value : Value }

type alias Env = List Binding

type alias ExprCL = List ExprC

type alias AppCType =
  { func : ExprC,
    args : ExprCL }

type ExprC = 
  NumC Float | 
  StringC String |
  AppC ExprC ExprCL


type Value = 
  NumV Float |
  StringV String |
  ErrorV String |
  PrimVPlus |
  PrimVMinus |
  PrimVMult |
  PrimVDiv

----------------------------
-- Interpreting functions --
----------------------------

-- Interprets an AST --
interp : List String -> ExprC -> Value
interp env expr =
  case expr of 
    NumC n -> (NumV n)
    StringC s -> (StringV s)
    AppC appc_expr args ->
       case (interp env appc_expr) of
          StringV symbol ->
            case symbol of
              "+" -> (interp_plus (List.map (interp env) args))
              "-" -> (interp_minus (List.map (interp env) args))
              "*" -> (interp_mult (List.map (interp env) args))
              "/" -> (interp_div (List.map (interp env) args))
              _ -> (ErrorV "should match a prim function")
          _ -> (ErrorV "should match a symbol representing prim function")
      

-- Inteprets a mult, can take any amount of args
interp_mult : List Value -> Value
interp_mult values =
  case (List.head values) of
    Nothing -> (NumV 0)           -- these functions seem to match here... why?
    Just value ->
      case value of
        NumV n ->
          case (interp_mult (List.drop 1 values)) of
              NumV n2 -> (NumV (n * n2))
              _ -> (ErrorV "second in list should match a NumV")
        _ -> (ErrorV "first in list should match a NumV")


-- Inteprets a div, can take any amount of args
interp_div : List Value -> Value
interp_div values =
  case (List.head values) of
    Nothing -> (NumV 0)             -- these functions seem to match here... why?
    Just value ->
      case value of
        NumV n ->
          case (interp_div (List.drop 1 values)) of
              NumV n2 -> (NumV (n / n2))
              _ -> (ErrorV "second in list should match a NumV")
        _ -> (ErrorV "first in list should match a NumV")


-- Inteprets a plus, can take any amount of args
interp_plus : List Value -> Value
interp_plus values =
  case (List.head values) of
    Nothing -> (NumV 0)
    Just value ->
      case value of
        NumV n ->
          case (interp_plus (List.drop 1 values)) of
              NumV n2 -> (NumV (n + n2))
              _ -> (ErrorV "second in list should match a NumV")
        _ -> (ErrorV "first in list should match a NumV")

-- Inteprets a minus, can take any amount of args
interp_minus : List Value -> Value
interp_minus values =
  case (List.head values) of
    Nothing -> (NumV 0)
    Just value ->
      case value of
        NumV n ->
          case (interp_minus (List.drop 1 values)) of
              NumV n2 -> (NumV (n - n2))
              _ -> (ErrorV "second in list should match a NumV")
        _ -> (ErrorV "first in list should match a NumV")




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
  (check_equal (interp [] (NumC 4)) (NumV 4)),
  (check_equal (interp [] (StringC "hi")) (StringV "hi")),
  (check_equal (interp [] (AppC (StringC "+") [(NumC 2), (NumC 2)])) (NumV 4)),
  (check_equal (interp [] (AppC (StringC "-") [(NumC 2), (NumC 2)])) (NumV 0)),
  (check_equal (interp [] (AppC (StringC "*") [(NumC 2), (NumC 2)])) (NumV 4)), -- these fail for no reason
  (check_equal (interp [] (AppC (StringC "/") [(NumC 2), (NumC 2)])) (NumV 1)), -- these fail for no reason
  

  --- Environmnent Tests -- 
  (check_equal env_new []),
  (check_equal (env_extend env_new (Binding "first" (NumV 4))) [(Binding "first" (NumV 4))]),
  (check_equal (env_lookup test_env "second") (NumV 2)),
  (check_equal (env_lookup [] "dne") (ErrorV "Value not found in environment"))
  ]
 

main = Html.text (Debug.toString test_list)
  