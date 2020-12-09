import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class ParserTest {

    @Test
    fun testStringLiteralExpression() {
        val input = """"hello world";"""
        val lexer = newLexer(input)
        val parser = newParser(lexer)
        val program = parser.parseProgram()
        checkParseErrors(parser)

        val stmt = program.statements[0]
        assertTrue(stmt is AstExpressionStatement)
        assertTrue(stmt.expression is AstStringLiteral)
        val literal = stmt.expression as AstStringLiteral
        assertEquals(literal.value, "hello world")
    }

    @Test
    fun testLetStatements() {
        val testCases = listOf(
            Triple<String, String, Any>("let x = 5;", "x", 5),
            Triple<String, String, Any>("let y = true;", "y", true),
            Triple<String, String, Any>("let foobar = y;", "foobar", "y"),
        )

        for (testCase in testCases) {
            val lexer = newLexer(testCase.first)
            val parser = newParser(lexer)
            val program = parser.parseProgram()

            checkParseErrors(parser)

            val stmt = program.statements[0]
            testLetStatement(stmt, testCase.second)

            val letStatement = stmt as AstLetStatement
            testLiteralExpression(letStatement.value, testCase.third)
        }
    }

    private fun checkParseErrors(parser: Parser) {
        if (parser.errors.isNotEmpty()) {
            println(parser.errors)
            assertTrue { parser.errors.isEmpty() }
        }
    }

    private fun testLetStatement(s: AstStatement, name: String) {
        assertEquals("let", s.tokenLiteral())
        assertTrue(s is AstLetStatement)
        assertEquals(name, s.name.value)
        assertEquals(name, s.name.tokenLiteral())
    }

    private fun testLiteralExpression(exp: AstExpression?, expected: Any) {
        assertNotNull(exp)
        when (expected) {
            is Int -> testIntegerLiteral(exp, expected)
            is String -> testIdentifier(exp, expected)
            is Boolean -> testBooleanLiteral(exp, expected)
        }
    }

    private fun testIntegerLiteral(il : AstExpression, value : Int) {
        assertTrue(il is AstIntegerLiteral)
        assertEquals(il.value, value)
        assertEquals(il.tokenLiteral(), value.toString())
    }

    private fun testIdentifier(il : AstExpression, value : String) {
        assertTrue(il is AstIdentifier)
        assertEquals(il.value, value)
        assertEquals(il.tokenLiteral(), value)
    }

    private fun testBooleanLiteral(il : AstExpression, value : Boolean) {
        assertTrue(il is AstBoolean)
        assertEquals(il.value, value)
        assertEquals(il.tokenLiteral(), value.toString())
    }
}