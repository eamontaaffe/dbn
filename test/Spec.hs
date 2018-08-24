import Test.Hspec
import Test.QuickCheck

import Dbn

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "Should tokenize some simple examples" $ do
      shouldBe
        (lexer "Paper 100")
        [TokenWord "Paper", TokenNumber 100]

      shouldBe
        (lexer "Pen 100")
        [ TokenWord "Pen"
        , TokenNumber 100
        ]

      shouldBe
        (lexer "Line 0 50 100 50")
        [ TokenWord "Line"
        , TokenNumber 0
        , TokenNumber 50
        , TokenNumber 100
        , TokenNumber 50
        ]

  describe "parser" $ do
    it "Should parse the tokens into a tree" $ do
      shouldBe
        (parser [TokenWord "Paper", TokenNumber 100])
        (Drawing [CallExpression "Paper" [NumberLiteral 100]])

      shouldBe
        (parser [TokenWord "Pen", TokenNumber 100])
        (Drawing [CallExpression "Pen" [NumberLiteral 100]])

      shouldBe
        (parser [ TokenWord "Line"
                , TokenNumber 0
                , TokenNumber 50
                , TokenNumber 100
                , TokenNumber 50
                ])
        (Drawing [CallExpression "Line" [ NumberLiteral 50
                                        , NumberLiteral 100
                                        , NumberLiteral 50
                                        , NumberLiteral 0
                                        ]])


  describe "transformer" $ do
    it "Should transform the AST into a canvas" $ do
      shouldBe
        (Canvas 100 [])
        (transformer (Drawing [ CallExpression "Paper" [ NumberLiteral 100 ]]))

      shouldBe
        (Canvas 0 [])
        (transformer (Drawing [ CallExpression "Paper" [ NumberLiteral 0 ]]))

      shouldBe
        (Canvas 100 [])
        (transformer (Drawing []))

      shouldBe
        (Canvas 100 [Line 100 0 50 100 50])
        (transformer (Drawing [ CallExpression "Pen" [ NumberLiteral 100 ]
                              , CallExpression "Line" [ NumberLiteral 50
                                                      , NumberLiteral 100
                                                      , NumberLiteral 50
                                                      , NumberLiteral 0
                                                      ]
                              ]))

      shouldBe
        (Canvas 100 [Line 0 0 50 100 50])
        (transformer (Drawing [ CallExpression "Pen" [ NumberLiteral 0 ]
                              , CallExpression "Line" [ NumberLiteral 50
                                                      , NumberLiteral 100
                                                      , NumberLiteral 50
                                                      , NumberLiteral 0
                                                      ]
                              ]))


  describe "generator" $ do
    it "Should generate an svg from a canvas" $ do
      shouldBe
        ""
        (generator (Canvas 100 []))

      
