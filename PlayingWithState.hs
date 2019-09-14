
    import Control.Monad.State

    type Pattern = [[Int]]
    type PatternState = (Position,Pattern)
    type Depth = Int
    type Position = Int

    type Side = Char
    type Box = [Int]

    type StateBox = (Side,Box)
    
    -- creates a [[Int]] 
    --[1]
    --[1,1]
    --[1,1,1]
    
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
    
import Control.Monad.State
type Position = Char
type Box = [Int]
type StateBox = (Position,Box)
 
--adds 1s on both sides of a list 
addones::Box->State StateBox Box
addones [] = do
  (_,box)<- get
  return [sum(box)]
addones list = do
   (pos,box)<-get
   let sumbox = [sum box]
       len =length box
       evalSTaddons a = (evalState (addones $ tail a) ('R',[]))
   if head list == 1 then    
       do put ('L',box++[1])
          addones $ tail list                    
   else
       if len > 1 then
           if pos == 'L' then
              do
                let new = sumbox++list
                    next = evalSTaddons new
                return $ (reverse $ drop (next!!0) $ reverse new)++next
           else
              return (list++sumbox)
       else
           if pos == 'L' then
             return $ [head list]++evalSTaddons list
           else
              return $ evalSTaddons list
 
 
main = putStrLn $ show $ evalState(addones [1,1,1,2,2,1,1]) ('L',[])
    
        
        



    
