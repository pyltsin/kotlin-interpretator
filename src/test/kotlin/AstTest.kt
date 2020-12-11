import kotlin.test.Test
import kotlin.test.assertEquals

class AstTest {
    @Test
    fun testString() {
        val program = AstProgram(
            mutableListOf(
                AstLetStatement(
                    token = Token(literal = "let", type = TokenType.LET),
                    name = AstIdentifier(token = Token(literal = "myVar", type = TokenType.IDENT), value = "myVar"),
                    value = AstIdentifier(
                        token = Token(literal = "anotherVar", type = TokenType.IDENT),
                        value = "anotherVar"
                    )
                )
            )
        )
        assertEquals("let myVar = anotherVar;", program.string().trim())
    }
}