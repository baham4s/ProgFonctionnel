import System.IO

getCh :: IO Char
getCh = do
        hSetEcho stdin False
        c <- getChar
        hSetEcho stdin True
        return c

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

match :: String -> String -> String
match xs ys = [if elem x ys 
                 then x
                 else '-' | x <- xs]

play :: String -> IO ()
play a = do putChar '?'
            tmp <- getLine
            if tmp == a 
               then putStrLn "You got it!"
            else                       
                do putStrLn (match a tmp)
                   play a

hangman :: IO ()
hangman = do putStrLn "Think of a word : "
             word <- sgetLine
             putStrLn "Try to guess it : "
             play word

main = hangman
            
            
