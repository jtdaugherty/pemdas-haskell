module Pemdas.Parse
-- JTD:
--
-- I noted something along these lines in Main.hs, but unless you want
-- this module to export a parser specifically be composed with other
-- Parsec parsers, it would probably be better to hide the parser
-- entirely and just export a function parseMyStuff :: String -> Either
-- Error Stuff. That keeps the parsing operation abstract and reduces
-- API burden for the caller.
    (makeExprParser)
where

import qualified Data.Functor.Identity

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string')
import qualified Text.Parsec.Token as Token
import Text.Parsec ( (<|>), oneOf, letter, alphaNum, many, try )

import qualified Pemdas.Types as PT
import Text.Parsec.Perm (permute, (<$?>), (<|?>))


-- Parser

makeExprParser :: [(String, PT.BinOp Double)] -> Parser (PT.Expr Double)
makeExprParser binOps = expr
    where
    expr :: Parser (PT.Expr Double)
    expr = do
        whiteSpace
        infixChain

    term :: Parser (PT.Expr Double)
    term =
        -- JTD:
        --
        -- Some of these alternatives are long enough that I recommend
        -- pulling them out as top-level declarations. That will make
        -- this very concise and easy to understand, and it will also
        -- make reordering them (if that's ever deemed appropriate) a
        -- matter of just moving a line or two around, and that will
        -- result in smaller commits that are easier to understand.
        parens expr
            <|> simpleExpr
            <|> (PT.Negated <$> (char '-' >> expr))
            -- TODO this approach leads to bad error messages.
            -- Would be better to diverge later if we can
            <|> try (do
                    -- JTD:
                    --
                    -- This can also be written
                    --
                    -- void $ char '\\'
                    _ <- char '\\'
                    aggName <- identifier'
                    _ <- char '_'
                    (varName, domainMin, domainMax) <- braces quantification
                    bodyExpr <- parens expr
                    return $ PT.Quantified $ PT.AggregationApplication
                        { PT.aggName   = aggName
                        , PT.varName   = varName
                        , PT.domainMin = domainMin
                        , PT.domainMax = domainMax
                        , PT.aggBody   = bodyExpr
                        })
            <|> do
                    _ <- char '\\'
                    funcName <- identifier'
                    (subscriptExpr, superscriptExpr) <- subscriptAndSuperscript
                    argExpr <- parens expr
                    return $ PT.ApplyFunc $ PT.FunctionApplication
                        { PT.funcName        = funcName
                        , PT.subscriptExpr   = subscriptExpr
                        , PT.superscriptExpr = superscriptExpr
                        , PT.argumentExpr    = argExpr
                        }

    subscriptAndSuperscript ::
        Parser (Maybe (PT.Expr Double), Maybe (PT.Expr Double))
    subscriptAndSuperscript =
        permute $ (,) <$?> (Nothing, Just <$> subscript)
                      <|?> (Nothing, Just <$> superscript)

    subscript :: Parser (PT.Expr Double)
    subscript =
        char '_'
        >> (simpleExpr <|> braces expr)
    superscript :: Parser (PT.Expr Double)
    superscript =
        char '^'
        >> (simpleExpr <|> braces expr)

    -- Expressions that don't need parens in superscript/subscript
    simpleExpr :: Parser (PT.Expr Double)
    simpleExpr =
        (PT.Literal <$> literal)
        <|> (PT.Variable <$> identifier)
    -- JTD:
    --
    -- The line after this one has trailing whitespace.
    
    precedenceTable :: [(String, Int)]
    precedenceTable = [(opText, prec) | (opText, (prec, _)) <- binOps]

    infixChain :: Parser (PT.Expr Double)
    infixChain = do
        firstTerm <- term
        triples <-
            many $ do
                op <- operator
                nextTerm <- term
                -- JTD:
                --
                -- I suggest pulling this triple out and giving it a let-bound name.
                return
                    -- JTD:
                    --
                    -- Equivalent to
                    --
                    -- fromMaybe (-1) $ lookup op precedenceTable
                    ( case lookup op precedenceTable of
                        Just n -> n
                        Nothing -> -1
                    , op
                    , nextTerm
                    )
        return $ groupByPrecedence firstTerm triples

-- groupByPrecedence, together with shunt, implements the Shunting-yard
-- algorithm for grouping infix expressions by precedence.
groupByPrecedence ::
    (Num a, Show a) => PT.Expr a -> [(Int, String, PT.Expr a)] -> PT.Expr a
groupByPrecedence firstTerm triples =
    let (remainingStack, lastTerm) =
            foldl shunt ([], firstTerm) triples
    in foldl
        (\rightTerm (_, op, leftTerm) -> PT.Infix op leftTerm rightTerm)
        lastTerm
        remainingStack

shunt ::
    Num a =>
    ([(Int, String, PT.Expr a)], PT.Expr a)
    -> (Int, String, PT.Expr a)
    -> ([(Int, String, PT.Expr a)], PT.Expr a)
shunt ([], term1) (prec1, op1, term2) =
    ([(prec1, op1, term1)], term2)
shunt ((prec0, op0, term0):stack, term1) (prec1, op1, term2)
    | prec1 <= prec0 =
        case stack of
            (prec0', op0', term0'):stack' ->
                shunt
                    ((prec0', op0', term0'):stack', PT.Infix op0 term0 term1)
                    (prec1, op1, term2)
            _ -> ((prec1, op1, PT.Infix op0 term0 term1):stack, term2)
    | otherwise =
        ((prec1, op1, term1):(prec0, op0, term0):stack, term2)

quantification :: Parser (String, Integer, Integer)
quantification =
    do
        varName <- identifier
        _ <- string' "\\in"
        whiteSpace
        (domainMin, domainMax) <- brackets $
            do
                domainMin <- integer
                _ <- string' ".."
                domainMax <- integer
                return (domainMin, domainMax)
        return (varName, domainMin, domainMax)


-- Lexer

languageDef :: Token.GenLanguageDef String u Data.Functor.Identity.Identity
languageDef = emptyDef
    { Token.reservedNames = ["in"]
    , Token.opStart       = Token.opLetter languageDef
    , Token.opLetter      = oneOf ":!#$%&*+./<=>?@|-~"
    , Token.identStart    = letter
    , Token.identLetter   = alphaNum
    }
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

-- lexeme :: Parser a -> Parser a
-- lexeme = Token.lexeme lexer

literal :: Parser Double
literal = do
    natOrFloat <- Token.naturalOrFloat lexer
    case natOrFloat of
        Left n -> return $ fromIntegral n
        Right d -> return d

-- TODO this integer parser supports leading +, but that's silly here
integer :: Parser Integer
integer = Token.integer lexer

identifier :: Parser String
identifier = Token.identifier lexer

-- Non-lexeme identifier parser
identifier' :: Parser String
identifier' = do
    c <- letter
    cs <- many letter
    return (c:cs)

operator :: Parser String
operator = Token.operator lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
