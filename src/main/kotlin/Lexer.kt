class Lexer(private val input: String) {
    private var position: Int = 0
    private var readPosition: Int = 0
    private var ch: String = ""

    fun nextToken(): Token {

        skipWhitespace()
        val token: Token = when (ch) {
            "=" -> if (peekChar() == "=") {
                val temp = ch
                readChar()
                newToken(TokenType.EQ, temp + ch)
            } else {
                newToken(TokenType.ASSIGN, ch)
            }
            "+" -> newToken(TokenType.PLUS, ch)
            "-" -> newToken(TokenType.MINUS, ch)
            "!" -> if (peekChar() == "=") {
                val temp = ch
                readChar()
                newToken(TokenType.NOT_EQ, temp + ch)
            } else {
                newToken(TokenType.BANG, ch)
            }
            "/" -> newToken(TokenType.SLASH, ch)
            "*" -> newToken(TokenType.ASTERISK, ch)
            "<" -> newToken(TokenType.LT, ch)
            ">" -> newToken(TokenType.GT, ch)
            ";" -> newToken(TokenType.SEMICOLON, ch)
            "," -> newToken(TokenType.COMMA, ch)
            "{" -> newToken(TokenType.LBRACE, ch)
            "}" -> newToken(TokenType.RBRACE, ch)
            "(" -> newToken(TokenType.LPAREN, ch)
            ")" -> newToken(TokenType.RPAREN, ch)
            "\"" -> newToken(TokenType.STRING, readString())
            "[" -> newToken(TokenType.LBRACKET, ch)
            "]" -> newToken(TokenType.RBRACKET, ch)
            ":" -> newToken(TokenType.COLON, ch)
            "." -> newToken(TokenType.DOT, ch)
            "" -> newToken(TokenType.EOF, "")
            else -> when {
                isLetter(ch) -> {
                    val identifier: String = readIdentifier()
                    return newToken(lookupIdent(identifier), identifier)
                }
                isDigit(ch) -> return newToken(TokenType.INT, readNumber())
                else -> newToken(TokenType.ILLEGAL, ch)
            }
        }
        readChar()
        return token
    }

    private fun readIdentifier(): String {
        val temp = position
        while (isLetter(ch)) {
            readChar()
        }
        return input.substring(temp, position)
    }

    private fun readNumber(): String {
        val temp = position
        while (isDigit(ch)) {
            readChar()
        }
        return input.substring(temp, position)
    }

    private fun isLetter(letter: String): Boolean {
        return letter[0] in 'a'..'z' || letter[0] in 'A'..'Z' || letter[0] == '_'
    }

    private fun isDigit(letter: String): Boolean {
        return letter[0] in '0'..'9'
    }

    private fun readString(): String {
        val temp = position + 1
        while (true) {
            readChar()
            if (ch == "\"" || ch == "") {
                break
            }
        }
        return input.substring(temp, position)
    }

    private fun peekChar(): String {
        return if (readPosition >= input.length) {
            ""
        } else {
            input[readPosition].toString()
        }
    }

    private fun skipWhitespace() {
        while (ch == " " || ch == "\t" || ch == "\n" || ch == "\r") {
            readChar()
        }
    }

    internal fun readChar() {
        ch = if (readPosition >= input.length) {
            ""
        } else {
            input[readPosition].toString()
        }

        position = readPosition
        readPosition++
    }
}

fun newLexer(input: String): Lexer {
    val lexer = Lexer(input)
    lexer.readChar()
    return lexer
}