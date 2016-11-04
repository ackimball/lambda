
[[functions-and-types-for-parsing]]
= Functions and types for parsing

In this file is the source and explanation for the parsing functions
which we've been using, and some limited notes about the wrappers and
full types in Parsec.

> module FunctionsAndTypesForParsing where
>
> import Text.Parsec (ParseError)
> import Text.Parsec.String (Parser)
> import Text.Parsec.String.Parsec (parse)
> import Text.Parsec.String.Char (oneOf)
> import Text.Parsec.String.Combinator (eof,manyTill,anyToken)
> import Control.Applicative ((<$>), (<*>), (<*), (*>), many)
> import Control.Monad (void)


== Functions for parsing

Here are the testing functions which were used earlier:

The basic parse function: this is a pretty simple wrapper. The parse
function from parsec just adds a filename to use in parse errors,
which is set as the empty string here.

> regularParse :: Parser a -> String -> Either ParseError a
> regularParse p = parse p ""

'parse' is a basic function in the family of functions for running
parsers in Parsec. You can compose the parser functions in the Parser
monad, then run the top level function using 'parse' and get back an
'Either ParserError a' as the result. There are a few alternatives to
'parse' in Parsec, mostly when you are using a more general parser
type instead of 'Parser a' (which is an alias for 'ParsecT String ()
Identity a'). Have a look in the Text.Parsec.Prim module for these
<http://hackage.haskell.org/package/parsec-3.1.3/docs/Text-Parsec-Prim.html>.

This function will run the parser, but additionally fail if it doesn't
consume all the input.

> parseWithEof :: Parser a -> String -> Either ParseError a
> parseWithEof p = parse (p <* eof) ""

This function will apply the parser, then also return any left over
input which wasn't parsed.

> parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
> parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
>   where leftOver = manyTill anyToken eof

TODO: what happens when you use 'many anyToken <* eof' variations
instead? Maybe should talk about greediness? Or talk about it in a
better place in the tutorial.

> parseWithWSEof :: Parser a -> String -> Either ParseError a
> parseWithWSEof p = parseWithEof (whiteSpace *> p)
>   where whiteSpace = void $ many $ oneOf " \n\t"
