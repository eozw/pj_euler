

main = do
  putStr "Q: "
  line <- getLine
  putStrLn line
  if line=="" then
    putStrLn "The program ended."
  else do
    main



