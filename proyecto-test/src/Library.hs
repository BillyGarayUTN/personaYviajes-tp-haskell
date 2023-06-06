module Library where
import PdePreludat

-- Firendsionales
-- Alias
type Yulius = Number
type Alegronios = Number
type NervioFrinas = Number

data Persona = Persona {
    nombre :: String,
    edad :: Number,
    nivelAlegria :: Alegronios,
    nivelAnsiedad :: NervioFrinas,
    tareas :: [String]
} deriving (Show)
-- TEST
billy = Persona {nombre= " billy", edad = 41, nivelAlegria = 400, nivelAnsiedad = 401 , tareas = [] }

nivelEnergia :: Persona -> Number
nivelEnergia persona
    | ((nivelAnsiedad persona) <) .nivelAlegria $ persona = (min 340).(*2).nivelAlegria $ persona 
    | (&& (edad persona < 40 )).  ((nivelAnsiedad persona) >).nivelAlegria $ persona  = (300-).nivelStrees $ persona
    | otherwise  = (+10).nivelEnergia $ persona

nivelStrees:: Persona-> Number
nivelStrees persona 
    | (5<).length.tareas $ persona = (1.5*).nivelAnsiedad $ persona 
    | otherwise                    = nivelAnsiedad $ persona 

