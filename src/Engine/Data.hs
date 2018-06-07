module Engine.Data (Engine(..)) where
    data Engine = Engine {
        step::Int,
        pos::Int,
        state::String,
        eaction::String,
        tape::[String]
    } deriving (Show)