{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Either (isLeft)

import Test.QuickCheck (quickCheckAll)

import Pemdas.Types


prop_showLiteral :: Double -> Bool
prop_showLiteral x = show (Literal x) == show x

prop_showVariable :: String -> Bool
prop_showVariable name = show (Variable name) == name


basicLanguage :: Language Integer
basicLanguage = Language
    { binOps       = [ ("/", (7, \m n -> if n == 0 then Left "division by 0"
                                                   else Right $ m `div` n))
                     , ("-", (6, \m n -> Right $ m - n))
                     ]
    , functions    = [("succ", \_ _ n -> Right $ succ n)]
    , aggregations = [("Sigma", Right . sum . map snd)]
    , env          = []
    }


prop_evalVarEmptyEnv :: String -> Bool
prop_evalVarEmptyEnv name = isLeft $ evaluateIn basicLanguage (Variable name)

-- JTD:
--
-- To be explicit: I see lots of top-level bindings' type signatures,
-- and I want to positively reinforce that as a practice. Some Haskell
-- programmers don't bother with those, although I don't see that much
-- at Galois. But please keep doing this! I find it helps not only
-- for readability but to force us to check our thinking on what we
-- think the type of a thing should be. And if you leave these out, the
-- compiler will guess the type, and it isn't always quite right.
prop_evalQuantification :: String -> Integer -> [(String, Integer)] -> Bool
prop_evalQuantification name domainMin env =
    -- JTD:
    --
    -- Three style notes:
    --
    -- Record type and value syntax typically has whitespace around
    -- the '=', so
    --
    -- aggName = "Sigma"
    --
    -- Second:
    --
    -- For readability, I recommend either putting the first record
    -- field and "{" on the same line as the constructor or, if that
    -- results in a line that you think is too long, make a local helper
    -- function that makes building these records less verbose.
    --
    -- Third:
    --
    -- The (Quantified ...) expression here is problematic for
    -- readability because it breaks up the line that contains it
    -- (starting with "evaluateIn"). I recommend pulling these bits
    -- out as 'let' bindings, i.e.,
    --
    -- let expr = Quantified $
    --            AggregationApplication { aggName = "Sigma"
    --                                   , varName = name
    --                                   , domainMin = domainMin
    --                                   , domainMax = domainMin + 1
    --                                   , aggBody = Variable name
    --                                   }
    --     expectedResult = Right $ domainMin + domainMin + 1
    --     actualResult = evaluateIn basicLanguage expr
    --
    -- in actualResult == expectedResult
    --
    -- In general, 'let' (or 'where') bindings are going to do wonders
    -- for resulting in more readable, concise, and clearer expressions,
    -- and they'll also help avoid indentation or parenthesizing
    -- headaches, both for reading and refactoring.
    evaluateIn basicLanguage (Quantified
        AggregationApplication
        { aggName="Sigma"
        , varName=name
        , domainMin=domainMin
        , domainMax=domainMin + 1
        , aggBody=Variable name
        }
    ) == Right (domainMin + domainMin + 1)

prop_fullEvaluation :: String -> String -> Integer -> Integer -> Bool
prop_fullEvaluation nameX nameY x domainMin =
    -- x + -\Sigma_{y \in [domainMin..domainMin+5]}(\succ(x + y))
    evaluateIn
        (basicLanguage { env = [(nameX, x)] })
        (Infix "+" (Variable nameX)
        -- JTD:
        --
        -- I recommend putting the dollar signs on the preceding lines, i.e.,
        --
        -- Negated $
        -- Quantified $
        -- ...
        $ Negated
        $ Quantified
        -- JTD:
        --
        -- Another spot where I'd recommend a change of record syntax as
        -- well as a 'let' binding.
        $ AggregationApplication
            { aggName="Sigma"
            , varName=nameY
            , domainMin=domainMin
            , domainMax=domainMin + 5
            , aggBody=ApplyFunc
                FunctionApplication
                    { funcName="succ"
                    , subscriptExpr=Nothing
                    , superscriptExpr=Nothing
                    , argumentExpr=Infix "+" (Variable nameX) (Variable nameY)
                    }
            })
    == Right (x - sum [succ (x + y) | y <- [domainMin..domainMin+5]])


return [] -- TemplateHaskell splice for quickCheckAll

main = $quickCheckAll
