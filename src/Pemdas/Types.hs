{-# LANGUAGE NamedFieldPuns #-}
module Pemdas.Types
    ( Expr(..)
    , BinOp
    , Function
    , FunctionApplication(..)
    , Aggregation
    , AggregationApplication(..)
    , evaluateIn
    , Language(..)
    )
where


-- JTD:
--
-- Style note: as with record values, for readability I recommend always
-- putting the constructor name on the same line as the first record
-- field. Breaking those up can be jarring for readers since the fields
-- go together with the constructor, and in some cases you might want
-- more than one record constructor in the same type, in which case that
-- style makes for harder reading. (Although there are other pitfalls
-- with declaring record types with multiple constructors that we could
-- discuss.)
--
-- data Num a => Language a =
--     Language { binOps ...
--              ...
--              }
--     | OtherConstr { ...
--                   }
data Num a => Language a = Language
    { binOps :: [(String, BinOp a)]
    , functions :: [(String, Function a)]
    , aggregations :: [(String, Aggregation a)]
    , env :: [(String, a)]
    }


data Num a => Expr a =
    Literal a
    | Variable String
    | Negated (Expr a)
    -- JTD:
    --
    -- It looks like you want to be able to register new infix operators
    -- dynamically. If so, I recommend using a newtype here instead
    -- of String, so as to avoid possible (and likely) bugs where you
    -- are holding a String and don't know for sure whether it's a
    -- legitimate operator.
    | Infix String (Expr a) (Expr a)
    | ApplyFunc (FunctionApplication a)
    | Quantified (AggregationApplication a)

-- JTD:
--
-- Type aliases often hinder readability because a reader will see a
-- BinOp and think that's a first-class value, and then they'll see it
-- get used as if it were a tuple. Instead of making an alias, I suggest
-- just going ahead and making this a record type; you even already have
-- the field names written down!
type BinOp a = (Int, a -> a -> Either String a)  -- (precedence, logic)

-- JTD:
--
-- Parens are redundant here.
type Function a = (Maybe a -> Maybe a -> a -> Either String a)
-- JTD:
--
-- Same record style change recommended here.
data FunctionApplication a = FunctionApplication
    { funcName :: String
    , subscriptExpr :: Maybe (Expr a)
    , superscriptExpr :: Maybe (Expr a)
    , argumentExpr :: Expr a
    }
-- JTD:
--
-- Parens are redundant here.
type Aggregation a = ([(Integer, a)] -> Either String a)
-- JTD:
--
-- Same record style change recommended here.
data AggregationApplication a = AggregationApplication
    { aggName :: String
    , varName :: String
    , domainMin :: Integer
    , domainMax :: Integer
    , aggBody :: Expr a
    }

-- JTD:
--
-- Show is generally understood to mean "the valid Haskell syntax of a
-- value that could be used to reconstruct it" and is expected to be
-- the inverse of Read. As written here, this is not a conventional
-- Show instance: it's a pretty-printer instead. I recommend migrating
-- this out of Show, deriving Show on the types instead (since it
-- can be useful to get derived Show instances to sidestep your
-- possibly-buggy pretty-printer when debugging), and then maybe using
-- the 'prettyprinter' library (or similar) to build a first-class
-- pretty-printer for your language. The combinators in such packages
-- make that sort of thing *very* easy and you can get really nice
-- results.
instance (Num a, Show a) => Show (Expr a) where
    -- Implementing this w/ showsPrec could be more efficient
    show (Literal x) = show x
    show (Variable name) = name
    show (Negated expr) = "-" ++ show expr
    show (Infix opText leftExpr rightExpr) =
        "(" ++ show leftExpr ++ " " ++ opText ++ " " ++ show rightExpr ++ ")"
    show
        (ApplyFunc FunctionApplication
            { funcName
            , subscriptExpr
            , superscriptExpr
            , argumentExpr
            }) =
        "\\"
        ++ funcName
        ++ case subscriptExpr of
            Nothing -> ""
            Just (Literal x) -> "_" ++ show x
            Just (Variable name) -> "_" ++ name
            Just expr -> "_{" ++ show expr ++ "}"
        ++ case superscriptExpr of
            Nothing -> ""
            Just (Literal x) -> "^" ++ show x
            Just (Variable name) -> "^" ++ name
            Just expr -> "^{" ++ show expr ++ "}"
        ++ "(" ++ show argumentExpr ++ ")"
    show
        (Quantified AggregationApplication
            { aggName
            , varName
            , domainMin
            , domainMax
            , aggBody
            }) =
        "\\" ++ aggName ++ "_{" ++ varName ++ " \\in ["
        ++ show domainMin ++ ".." ++ show domainMax ++ "]}("
        ++ show aggBody ++ ")"


-- JTD:
--
-- The pattern-match on Language should be on one line here (as is the
-- case in general with function argument pattern-matching), and if that
-- results in a line that's too long for your taste (even with field
-- punning) then I commend just writing this instead:
--
-- evaluateIn lang =
evaluateIn :: Num a => Language a -> Expr a -> Either String a
evaluateIn (Language
    { binOps
    , functions
    , aggregations
    , env
    }) = evalInEnv env
    where
    -- JTD:
    --
    -- Some of these 'where' bindings are large enough (in LOC or
    -- complexity) that I recommend pulling them out as top-level
    -- (unexported) declarations. That will also cut down on extraneous
    -- indentation that is a consequence of them being in the 'where'
    -- clause here.
    evalInEnv env' =
        -- JTD:
        --
        -- The first pattern-match of evalExpr probably ought to be
        -- rewritten as 'return x' since that is the monadic convention
        -- when using Either as a monad as is done here. I mention
        -- this because some of the other evalExpr alternatives are
        -- written in a monadic style. One reason to think about doing
        -- this is because if you use the generic Monad and MonadFail
        -- interfaces, you can change your monad type later with very
        -- little updates to the body of this function, in case you want
        -- to change your error propagation semantics or something like
        -- that.
        let evalExpr (Literal x) = Right x
            evalExpr (Variable name) =
                case lookup name env' of
                    -- JTD:
                    --
                    -- Written monadically as:
                    --
                    -- return x
                    Just x -> Right x
                    -- JTD:
                    --
                    -- Written monadically using MonadFail as:
                    --
                    -- fail $ "Undefined variable: " ++ name
                    Nothing -> Left $ "Undefined variable: " ++ name
            evalExpr (Negated expr) = negate <$> evalExpr expr
            evalExpr (Infix opText leftExpr rightExpr) =
                do
                    opFunc <- case lookup opText binOps of
                        -- JTD:
                        --
                        -- return op
                        Just (_, op) -> Right op
                        -- JTD:
                        --
                        -- fail $ ...
                        Nothing -> Left $ "Unknown infix operator: " ++ opText
                    leftArg <- evalExpr leftExpr
                    rightArg <- evalExpr rightExpr
                    opFunc leftArg rightArg
            evalExpr
                -- JTD:
                --
                -- Another multi-line pattern-match that should be on
                -- one line. I won't annotate any more of these since
                -- I've mentioned this a few times.
                (ApplyFunc FunctionApplication
                    { funcName
                    , subscriptExpr
                    , superscriptExpr
                    , argumentExpr
                    }) =
                do
                    func <- case lookup funcName functions of
                        Just f -> Right f
                        Nothing -> Left $ "Unknown function: " ++ funcName
                    subscript <- evalMaybeExpr subscriptExpr
                    superscript <- evalMaybeExpr superscriptExpr
                    arg <- evalExpr argumentExpr
                    func subscript superscript arg
            evalExpr
                (Quantified AggregationApplication
                    { aggName
                    , varName
                    , domainMin
                    , domainMax
                    , aggBody
                    }) =
                do
                    aggFunc <- case lookup aggName aggregations of
                        Just f -> Right f
                        Nothing -> Left $ "Unknown aggregation: " ++ aggName
                    let domain = [domainMin..domainMax]
                    bodyValues <-
                        -- JTD:
                        --
                        -- sequence will do here, but I think a more
                        -- readable approach is to use forM. See also
                        -- mapM, mapM_, and forM_.
                        --
                        -- forM domain $ \arg ->
                        --     evalInEnv ((varName, fromIntegral arg):env') aggBody
                        sequence
                            [evalInEnv ((varName, fromIntegral arg):env')
                                aggBody | arg <- domain]
                    aggFunc $ zip domain bodyValues

            evalMaybeExpr Nothing = Right Nothing
            evalMaybeExpr (Just expr) = Just <$> evalExpr expr
        in evalExpr
