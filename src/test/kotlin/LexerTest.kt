import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest {

    @Test
    fun testLexer() {
        val input = """
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
              x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
            	return true;
            } else {
            	return false;
            }

            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
            [1, 2];
            {"foo": "bar"}
            "test".len()
            test.len()
        """.trimIndent()

        val expectedTokens = listOf(
            TokenType.LET to "let",
            TokenType.IDENT to "five",
            TokenType.ASSIGN to "=",
            TokenType.INT to "5",
            TokenType.SEMICOLON to ";",
            TokenType.LET to "let",
            TokenType.IDENT to "ten",
            TokenType.ASSIGN to "=",
            TokenType.INT to "10",
            TokenType.SEMICOLON to ";",
            TokenType.LET to "let",
            TokenType.IDENT to "add",
            TokenType.ASSIGN to "=",
            TokenType.FUNCTION to "fn",
            TokenType.LPAREN to "(",
            TokenType.IDENT to "x",
            TokenType.COMMA to ",",
            TokenType.IDENT to "y",
            TokenType.RPAREN to ")",
            TokenType.LBRACE to "{",
            TokenType.IDENT to "x",
            TokenType.PLUS to "+",
            TokenType.IDENT to "y",
            TokenType.SEMICOLON to ";",
            TokenType.RBRACE to "}",
            TokenType.SEMICOLON to ";",
            TokenType.LET to "let",
            TokenType.IDENT to "result",
            TokenType.ASSIGN to "=",
            TokenType.IDENT to "add",
            TokenType.LPAREN to "(",
            TokenType.IDENT to "five",
            TokenType.COMMA to ",",
            TokenType.IDENT to "ten",
            TokenType.RPAREN to ")",
            TokenType.SEMICOLON to ";",
            TokenType.BANG to "!",
            TokenType.MINUS to "-",
            TokenType.SLASH to "/",
            TokenType.ASTERISK to "*",
            TokenType.INT to "5",
            TokenType.SEMICOLON to ";",
            TokenType.INT to "5",
            TokenType.LT to "<",
            TokenType.INT to "10",
            TokenType.GT to ">",
            TokenType.INT to "5",
            TokenType.SEMICOLON to ";",
            TokenType.IF to "if",
            TokenType.LPAREN to "(",
            TokenType.INT to "5",
            TokenType.LT to "<",
            TokenType.INT to "10",
            TokenType.RPAREN to ")",
            TokenType.LBRACE to "{",
            TokenType.RETURN to "return",
            TokenType.TRUE to "true",
            TokenType.SEMICOLON to ";",
            TokenType.RBRACE to "}",
            TokenType.ELSE to "else",
            TokenType.LBRACE to "{",
            TokenType.RETURN to "return",
            TokenType.FALSE to "false",
            TokenType.SEMICOLON to ";",
            TokenType.RBRACE to "}",
            TokenType.INT to "10",
            TokenType.EQ to "==",
            TokenType.INT to "10",
            TokenType.SEMICOLON to ";",
            TokenType.INT to "10",
            TokenType.NOT_EQ to "!=",
            TokenType.INT to "9",
            TokenType.SEMICOLON to ";",
            TokenType.STRING to "foobar",
            TokenType.STRING to "foo bar",
            TokenType.LBRACKET to "[",
            TokenType.INT to "1",
            TokenType.COMMA to ",",
            TokenType.INT to "2",
            TokenType.RBRACKET to "]",
            TokenType.SEMICOLON to ";",

            TokenType.LBRACE to "{",
            TokenType.STRING to "foo",
            TokenType.COLON to ":",
            TokenType.STRING to "bar",
            TokenType.RBRACE to "}",
            //	"test".len()
            TokenType.STRING to "test",
            TokenType.DOT to ".",
            TokenType.IDENT to "len",
            TokenType.LPAREN to "(",
            TokenType.RPAREN to ")",
            //	test.len()
            TokenType.IDENT to "test",
            TokenType.DOT to ".",
            TokenType.IDENT to "len",
            TokenType.LPAREN to "(",
            TokenType.RPAREN to ")",
            TokenType.EOF to "",
        )

        val lexer = newLexer(input)
        for (expected in expectedTokens) {
            val token: Token = lexer.nextToken()
            assertEquals(expected.first, token.tokenType)
            assertEquals(expected.second, token.literal)
        }
    }
}