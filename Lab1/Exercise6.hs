module Exercise6 where

main :: IO ()
main = do
    let guilty = (filter (\suspect -> length (accusers suspect) == 3) boys)
    case guilty of 
        [suspect] -> do 
            print suspect
            let honest = accusers suspect
            print honest
        _ -> putStrLn "Expected exactly one value"

data Boy = Matthew | Peter | Jack | Arnold | Carl
 deriving (Eq,Show)

 

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew suspect =
  suspect /= Carl && suspect /= Matthew
accuses Peter suspect =
  suspect == Matthew || suspect == Jack
accuses Jack suspect =
  not (accuses Matthew suspect) &&
  not (accuses Peter suspect)
accuses Arnold suspect =
  accuses Matthew suspect /= accuses Peter suspect  
accuses Carl suspect =
  not (accuses Arnold suspect)

accusers :: Boy -> [Boy]
accusers suspect = filter (\boy -> accuses boy suspect) boys