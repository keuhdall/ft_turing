module Engine.Data (Engine(..)) where
    data Engine = Engine {
        step::Int,
        pos::Int,
        initpos::Int,
        state::String,
        tape::[String]
    } deriving (Show)
