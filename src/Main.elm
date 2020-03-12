module Main exposing (..)
import Dict
import Html

type alias Binding =
  { name : Char,
    value : Value }

type alias Env = List Binding

type alias ExprCL = List ExprC
type alias SymbolL = List Char

type alias ValueL = List Value

type alias AppCType =
  { func : ExprC,
    args : ExprCL }


type ExprC =
  NumC Float |
  CharC Char |
  StringC String |
  LamC SymbolL ExprC |
  AppC ExprC ExprCL


type Value =
  NumV Float |
  StringV String |
  BoolV Bool |
  PrimV Char |
  ErrorV String |
  ClosV SymbolL ExprC Env


----------------------------
-- Interpreting functions --
----------------------------

-- Interprets an AST --
interp : Env -> ExprC -> Value
interp env expr =
  case expr of
    NumC n -> (NumV n)
    CharC c -> (env_lookup env c)
    StringC s -> (StringV s)
    LamC args body -> (ClosV args body env)
    AppC appc_expr args ->
      case (interp env appc_expr) of
        PrimV symbol ->
          case symbol of
            '+' -> (interp_plus (List.map (interp env) args))
            '-' -> (interp_minus (List.map (interp env) args))
            '*' -> (interp_mult (List.map (interp env) args))
            '/' -> (interp_div (List.map (interp env) args))
            _ -> (ErrorV "should match a prim function")
        ClosV c_args c_body c_env ->
          (interp
            (arg_list c_args
              (List.map (\arg -> (interp env arg)) args) c_env)
            c_body)
        _ -> (ErrorV "should match a symbol representing prim function")


arg_list : SymbolL -> ValueL -> Env -> Env
arg_list syms exprs env =
  if List.isEmpty syms && List.isEmpty exprs then
    env
  else
    let (s, e) = ((List.head syms), (List.head exprs)) in
      case s of
        Just sym ->
          case e of
            Just expr ->
              (Binding sym expr) :: (arg_list (List.drop 1 syms) (List.drop 1 exprs) env)
            _ -> env
        _ -> env


-- Inteprets a mult, can take any amount of args
interp_mult : List Value -> Value
interp_mult values =
  case (List.head values) of
    Nothing -> (NumV 0)          -- these functions seem to match here... why?
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
    Nothing -> (NumV 0)            -- these functions seem to match here... why?
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
env_lookup : Env -> Char -> Value
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

test_env = (env_extend (env_extend env_new (Binding 'f' (NumV 1))) (Binding 's' (NumV 2)))
top_env = [{name = '+', value = (PrimV '+')}, {name = '-', value = (PrimV '-')}, {name = '*', value = (PrimV '*')},
            {name = '/', value = (PrimV '/')}]

-- Add all tests here
test_list = [
  -- Interpretation Tests --
  (check_equal (interp [] (NumC 4)) (NumV 4)),
  (check_equal (interp [] (StringC "hi")) (StringV "hi")),
  (check_equal (interp top_env (AppC (CharC '+') [(NumC 2), (NumC 2)])) (NumV 4)),
  (check_equal (interp top_env (AppC (CharC '-') [(NumC 2), (NumC 2)])) (NumV 0)),
  (check_equal (interp top_env (AppC (CharC '*') [(NumC 2), (NumC 2)])) (NumV 4.0)), -- these fail for no reason
  (check_equal (interp top_env (AppC (CharC '/') [(NumC 2), (NumC 2)])) (NumV 1.0)), -- these fail for no reason
  (check_equal (interp top_env (AppC (LamC ['x', 'y'] (AppC (CharC '+') [(CharC 'y'), (CharC 'x')]))
                    [(AppC (CharC '+') [(NumC 13), (NumC 4)]), (NumC 12)])) (NumV 29)),
  (check_equal (interp top_env (AppC (LamC ['g'] (AppC (CharC 'g') [(NumC 10), (NumC 5)]))
    [(LamC ['a', 'b'] (AppC (CharC '+') [(CharC 'a'), (CharC 'b')]))])) (NumV 15)),

  --- arg list tests ---
  (check_equal (arg_list ['s', 'x'] [(NumV 2), (NumV 3)] []) [{ name = 's', value = (NumV 2)},
                                                              { name = 'x', value = (NumV 3)}]),

  --- Environmnent Tests --
  (check_equal env_new []),
  (check_equal (env_extend env_new (Binding 'f' (NumV 4))) [(Binding 'f' (NumV 4))]),
  (check_equal (env_lookup test_env 's') (NumV 2)),
  (check_equal (env_lookup [] 'd') (ErrorV "Value not found in environment"))
  ]


main = Html.text (Debug.toString test_list)
