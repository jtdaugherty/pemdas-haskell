-- JTD: for Main modules, we typically don't add an export list since
-- it's understood that 'main' is the only thing the compiler will care
-- about and no user will import this. Also, not adding an export list
-- in this specific case is helpful for debugging when you want to load
-- your Main in 'cabal repl' and try out things other than 'main'.
module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

-- JTD: I recommend ultimately hiding parsec from your library's public
-- API. Doing so keeps the parsing operation abstract and, particularly
-- if you wanted to release the library, means that your users don't
-- have to explicitly depend on parsec just to use your library (since
-- they'd need to import parsec, just as you are doing here). I'll say a
-- bit more about this below.
import Text.Parsec (eof, parse)

import Pemdas.Functions (doubleLanguage)
import Pemdas.Types (evaluateIn, Language (binOps))
import qualified Pemdas.Parse as P

-- TODO set up a real CLI with help text, etc
main :: IO ()
main = do
    args <- getArgs
    exprText <- case args of
        [exprText] -> return exprText
        clargs ->
            die $ "Exactly one argument is required; got "
            ++ show (length clargs)

    -- JTD:
    --
    -- parseResult is the thing I'd recommend moving into the library so
    -- no assembly of it is required here. If you were to do that, then
    -- that would help get rid of the parsec dependency and make the
    -- library more self-contained in case you wanted to do this same
    -- stuff in a different setting.
    --
    -- Style-wise, I would recommend more bindings to reduce indentation
    -- and expression complexity:
    --
    -- let parseResult = parse parser "" exprText
    --     parser = do
    --         parsedExpr <- P.makeExprParser $ binOps doubleLanguage
    --         eof
    --         return parsedExpr)
    --
    -- Also, parser can be simplified to:
    --
    --     parser = (P.makeExprParser $ binOps doubleLanguage) <* eof
    let parseResult =
            parse
                (do
                    parsedExpr <- P.makeExprParser $ binOps doubleLanguage
                    eof
                    return parsedExpr)
                "" exprText

    -- JTD:
    --
    -- Style-wise: I recommend writing the following pair of cases as
    -- follows in order avoid lots of explicit bindings and returns that
    -- run the risk of being noisy:
    --
    -- case parseResult of
    --     Left err ->
    --         die $ "Parsing failed " ++ show err
    --     Right expr -> do
    --         print expr
    --         case evaluateIn doubleLanguage expr of
    --             Left errMsg -> die $ "Error: " ++ errMsg
    --             Right result -> putStrLn $ "= " ++ show result
    --
    -- Also, this pattern of "case on an Either, die on a Left, continue
    -- on a Right, repeat" is exactly the kind of situation where the
    -- EitherT monad transformer would be useful, which I'd be happy to
    -- talk about more:
    --
    -- https://hackage.haskell.org/package/either-4.4.1.1/docs/Control-Monad-Trans-Either.html
    expr <- case parseResult of
        Left err ->
            die $ "Parsing failed " ++ show err
        Right expr -> return expr
    print expr
    result <- case evaluateIn doubleLanguage expr of
        Left errMsg -> die $ "Error: " ++ errMsg
        Right result -> return result
    putStrLn $ "= " ++ show result

    -- JTD: this return () is redundant since the type of putStrLn is IO ().
    return ()
