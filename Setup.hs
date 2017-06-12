import Distribution.Simple
main = do
    args <- getArgs
    let args' | "configure" `elem` args = args ++ ["--with-gcc","g++", "--with-ld","g++"]
              | otherwise               = args
    defaultMainArgs args'
