module Pemdas.Functions
    ( coreBinOps
    , floatingBinOps
    , integralBinOps
    , floatingFunctions
    , floatingEnv
    , coreAggregations
    , doubleLanguage
    )
where
import Pemdas.Types
    ( BinOp
    , Function
    , Aggregation
    , Language(Language, binOps, functions, aggregations, env)
    )

-- Helpers

cantFail2 :: (a -> b -> c) -> (a -> b -> Either String c)
cantFail2 f x y = Right $ f x y


-- JTD:
--
-- There are a few ways to break up a type signature like this. One way
-- that I like which makes it easy to line things up and makes it a bit
-- easier to write Haddock documentation for each argument type is:
--
-- withOptionalSubscript :: String
--                       -> (Maybe a -> a -> Either String a)
--                       -> (String, Function a)
--
-- Then it becomes possible to write inline Haddock like so:
--
-- withOptionalSubscript :: String
--                       -- ^ The string argument documentation
--                       -> (Maybe a -> a -> Either String a)
--                       -- ^ The function argument documentation
--                       -> (String, Function a)
withOptionalSubscript ::
    String -> (Maybe a -> a -> Either String a) -> (String, Function a)
withOptionalSubscript name wrappedFunc =
    (name, inner)
    where
        -- JTD:
        --
        -- Assuming the pattern ordering is still valid, failure cases
        -- are typically put last.
        inner _ (Just _) _ =
            Left $ "\\" ++ name ++ " does not accept superscript"
        inner subscript Nothing arg =
            wrappedFunc subscript arg

withOptionalSuperscript ::
    String -> (Maybe a -> a -> Either String a) -> (String, Function a)
withOptionalSuperscript name wrappedFunc =
    (name, inner)
    where
        inner (Just _) _ _ =
            Left $ "\\" ++ name ++ " does not accept subscript"
        inner Nothing superscript arg =
            wrappedFunc superscript arg

plainOnly :: String -> (a -> Either String a) -> (String, Function a)
plainOnly name wrappedFunc =
    (name, inner)
    where
        inner Nothing Nothing arg = wrappedFunc arg
        inner _ _ _ =
            Left $ "\\" ++ name ++ " does not accept subscript or superscript"


-- Function and global definitions for calculators

-- Binary operators
coreBinOps :: Num a => [(String, BinOp a)]
coreBinOps =
    [ ("+", (6, cantFail2 (+)))
    , ("-", (6, cantFail2 (-)))
    , ("*", (7, cantFail2 (*)))
    ]
-- JTD:
--
-- Typically one blank line between consecutive top-level declarations.
floatingBinOps :: Floating a => [(String, BinOp a)]
floatingBinOps =
    [ ("/",  (7, cantFail2 (/)))
    , ("**", (8, cantFail2 (**)))
    ]
integralBinOps :: Integral a => [(String, BinOp a)]
integralBinOps =
    -- JTD:
    --
    -- If you wanted to write the bodies of these ops monadically,
    -- here's how you could do it. This might be easier to read and/or
    -- extend and would be a bit more compact. But to some extent this
    -- comes down to whether you want to commit to a local convention of
    -- writing your evaluator's code in a monadic style everywhere. (I
    -- probably would.)
    --
    -- (7, \m n -> do
    --     when (n == 0) $ fail "modulo 0"
    --     return $ mod m n
    -- )
    [ ("%", (7, \m n -> if n == 0 then Left "modulo 0" else Right $ mod m n))
    , ( "//"
      , (7, \m n -> if n == 0 then Left "division by 0" else Right $ div m n)
      )
    , ( "^"
      , (8, \m n -> if n < 0 then Left "negative exponent" else Right (m ^ n))
      )
    ]

-- Functions

floatingFunctions :: Floating a => [(String, Function a)]
floatingFunctions =
    [ withOptionalSuperscript "sin"
        (\maybeSub arg ->
            -- JTD:
            --
            -- Perhaps 'maybeSub' should be called 'maybeSuper' here?
            --
            -- There's some repetition of values in the various floating
            -- functions that I'd probably pull out as follows.
            --
            -- This
            --
            -- Right $ case maybeSub of
            --     Nothing -> sin arg
            --     Just sub -> sin arg ** sub
            --
            -- would become
            --
            -- let maybeSuper = maybe id (flip (**)) maybeSub
            -- in return $ maybeSuper $ sin arg
            Right $ case maybeSub of
                Nothing -> sin arg
                Just sub -> sin arg ** sub
        )
    , withOptionalSuperscript "cos"
        (\maybeSub arg ->
            Right $ case maybeSub of
                Nothing -> cos arg
                Just sub -> cos arg ** sub
        )
    , withOptionalSuperscript "tan"
        (\maybeSub arg ->
            Right $ case maybeSub of
                Nothing -> tan arg
                Just sub -> tan arg ** sub
        )
    , plainOnly "exp" (Right <$> exp)
    , withOptionalSuperscript "sqrt"
        (\maybeSuper arg ->
            Right $ case maybeSuper of
                Nothing -> sqrt arg
                Just super -> arg ** (1 / super)
        )
    , withOptionalSubscript "log"
        (\maybeSub arg ->
            Right $ case maybeSub of
                Nothing -> log arg
                Just sub -> logBase sub arg
        )
    ]

floatingEnv :: Floating a => [(String, a)]
floatingEnv =
    [ ("pi", pi)
    , ("e", exp 1)
    ]

-- TODO add more of these (max, argmax, etc)

coreAggregations :: Num a => [(String, Aggregation a)]
coreAggregations =
    [ ("Sigma", Right . sum . map snd)
    , ("Pi",    Right . product . map snd)
    ]


doubleLanguage :: Language Double
doubleLanguage = Language
    { binOps       = coreBinOps ++ floatingBinOps
    , functions    = floatingFunctions
    , aggregations = coreAggregations
    , env          = floatingEnv
    }
