
    import Control.Monad.State

    type Pattern = [[Int]]
    type PatternState = (Position,Pattern)
    type Depth = Int
    type Position = Int

    type Side = Char
    type Box = [Int]

    type StateBox = (Side,Box)

    buildPattern::Depth->State PatternState Pattern
    buildPattern 0 = do
        (_,pattern)<-get
        return pattern
    buildPattern d = do
        (position,pattern)  <- get
        if (length $ last $ pattern) >= position then do
            if position == 1 then
                put (position+1,pattern)
            else
                put (position+1,pattern++[last pattern])
            buildPattern (d-1)
        else do
            let new = init pattern++[[1]++(last pattern)]
            if (length $ last $ new) < position then do
                let new' = init pattern++[(last new)++[1]]
                put (position,new')
                buildPattern(d)
            else do
                put (position+1,pattern++[last new])
                buildPattern (d-1)


    main = print $ evalState (buildPattern 5) (1,[[1]])
    
        
        



    