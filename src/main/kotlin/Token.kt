data class Token(
    val literal: String,
    val tokenType: TokenType,
)


private val keywords = mapOf(
    "fn" to TokenType.FUNCTION,
    "let" to TokenType.LET,
    "true" to TokenType.TRUE,
    "false" to TokenType.FALSE,
    "if" to TokenType.IF,
    "else" to TokenType.ELSE,
    "return" to TokenType.RETURN,
)

fun lookupIdent(ident: String): TokenType {
    val tokenType = keywords[ident]
    return tokenType ?: TokenType.IDENT
}

fun newToken(tokenType: TokenType, literal: String): Token {
    return Token(literal, tokenType)
}

enum class TokenType(var salue: String) {
    ILLEGAL("ILLEGAL"),
    EOF("EOF"),

    IDENT("IDENT"),
    INT("INT"),
    STRING("STRING"),

    // 1343456
    // Operators
    ASSIGN("="),
    PLUS("+"),
    MINUS("-"),
    BANG("!"),
    ASTERISK("*"),
    SLASH("/"),
    LT("<"),
    GT(">"),

    // Delimiters
    COMMA(","),
    SEMICOLON(";"),
    LPAREN("("),
    RPAREN(")"),
    LBRACE("{"),
    RBRACE("}"),
    LBRACKET("["),
    RBRACKET("]"),

    // Keywords
    FUNCTION("FUNCTION"),
    LET("LET"),
    TRUE("TRUE"),
    FALSE("FALSE"),
    IF("IF"),
    ELSE("ELSE"),
    RETURN("RETURN"),
    EQ("=="),
    NOT_EQ("!="),

    COLON(":"),
    DOT(".")
}
