structure while_astLrVals = while_astLrValsFun(
structure Token = LrParser.Token);

structure while_astLex = while_astLexFun(
structure Tokens = while_astLrVals.Tokens);

structure while_astParser = Join(
structure ParserData = while_astLrVals.ParserData
structure Lex=while_astLex
structure LrParser=LrParser);

