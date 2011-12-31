module PcfTyping  -- Define type substitutions as functions 
       ( PcfType (..)
       , PcfTerm (..)
       , TypeTemp (..)
       , TypeContext
       , typeInfer
       ) where 

-- Type for PCF types 
data PcfType = T_nat
             | T_bool
             | T_prod PcfType PcfType -- Product type
             | T_sum PcfType PcfType -- Sum type 
             | T_arrow PcfType PcfType -- Function type
             deriving (Eq)
               
-- Type for PCF terms                      
data PcfTerm = TM_var String -- Variables 
             | TM_num Int -- Numbers
             | TM_op PcfTerm PcfTerm -- Arithmetic operations
             | TM_true 
             | TM_false 
             | TM_eqtest PcfTerm PcfTerm -- Equality test of numbers
             | TM_if PcfTerm PcfTerm PcfTerm -- Conditional 
             | TM_pair PcfTerm PcfTerm -- Pair 
             | TM_projfst PcfTerm -- First projection 
             | TM_projsnd PcfTerm -- Second projection
             | TM_injfst PcfTerm -- First injection 
             | TM_injsnd PcfTerm -- Second injection 
             | TM_case PcfTerm (String, PcfTerm) (String, PcfTerm) -- Case distinction 
             | TM_abs String PcfTerm -- Lambda abstration (no type for bound variables)
             | TM_app PcfTerm PcfTerm -- Lambda application 
             | TM_fix PcfTerm -- Fix-point
             deriving (Eq, Show)
                      
-- Type for PCF type templates
data TypeTemp = TT_nat
              | TT_bool
              | TT_prod TypeTemp TypeTemp 
              | TT_sum TypeTemp TypeTemp
              | TT_arrow TypeTemp TypeTemp 
              | TT_var Int -- Type variables
              deriving (Eq)

type TypeContext = [(String, PcfType)]

constTypes = [T_nat, T_bool]
constTemps = [TT_nat, TT_bool]

instance Show (PcfType) where 
  show T_nat = "Nat" 
  show T_bool = "Bool" 
  show (T_prod t1 t2) =
    case (t1, t2) of _ | t1 `elem` constTypes && t2 `elem` constTypes -> show t1 ++ " * " ++ show t2
                       | t1 `elem` constTypes -> show t1 ++ " * (" ++ show t2 ++ ")"
                       | t2 `elem` constTypes -> "(" ++ show t1 ++ ") * " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (T_sum t1 t2) =
    case (t1, t2) of _ | t1 `elem` constTypes && t2 `elem` constTypes -> show t1 ++ " + " ++ show t2
                       | t1 `elem` constTypes -> show t1 ++ " + (" ++ show t2 ++ ")"
                       | t2 `elem` constTypes -> "(" ++ show t1 ++ ") + " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (T_arrow t1 t2) = 
    case (t1, t2) of (T_arrow _ _, _) -> "(" ++ show t1 ++ ") -> " ++ show t2
                     _ -> show t1 ++ " -> " ++ show t2
  
instance Show (TypeTemp) where 
  show TT_nat = "Nat" 
  show TT_bool = "Bool" 
  show (TT_var n) = "t" ++ show n
  show (TT_prod t1 t2) =
    case (t1, t2) of (TT_var _, TT_var _) -> show t1 ++ " * " ++ show t2                     
                     (TT_var _, _) | t2 `elem` constTemps -> show t1 ++ " * " ++ show t2
                                   | otherwise -> show t1 ++ " * (" ++ show t2 ++ ")"
                     (_, TT_var _) | t1 `elem` constTemps -> show t1 ++ " * " ++ show t2 
                                   | otherwise -> "(" ++ show t1 ++ ") * " ++ show t2
                     _ | t1 `elem` constTemps && t2 `elem` constTemps -> show t1 ++ " * " ++ show t2
                       | t1 `elem` constTemps -> show t1 ++ " * (" ++ show t2 ++ ")"
                       | t2 `elem` constTemps -> "(" ++ show t1 ++ ") * " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (TT_sum t1 t2) =
    case (t1, t2) of (TT_var _, TT_var _) -> show t1 ++ " + " ++ show t2                     
                     (TT_var _, _) | t2 `elem` constTemps -> show t1 ++ " + " ++ show t2
                                   | otherwise -> show t1 ++ " + (" ++ show t2 ++ ")"
                     (_, TT_var _) | t1 `elem` constTemps -> show t1 ++ " + " ++ show t2 
                                   | otherwise -> "(" ++ show t1 ++ ") + " ++ show t2
                     _ | t1 `elem` constTemps && t2 `elem` constTemps -> show t1 ++ " + " ++ show t2
                       | t1 `elem` constTemps -> show t1 ++ " + (" ++ show t2 ++ ")"
                       | t2 `elem` constTemps -> "(" ++ show t1 ++ ") + " ++ show t2
                       | otherwise -> "(" ++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (TT_arrow t1 t2) = 
    case (t1, t2) of (TT_arrow _ _, _) -> "(" ++ show t1 ++ ") -> " ++ show t2
                     _ -> show t1 ++ " -> " ++ show t2

typeInfer :: TypeContext -> PcfTerm -> Maybe TypeTemp
