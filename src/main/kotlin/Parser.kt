import java.util.*

enum class TypeOperation(val value: Int) {
    LOWEST(0),
    EQUALS(1),
    LESSGREATER(2),
    SUM(3),
    PRODUCT(4),
    PREFIX(5),
    CALL(6),
    DOT(7),
    INDEX(8),
}

val precedences = mapOf(
    TokenType.EQ to TypeOperation.EQUALS,
    TokenType.NOT_EQ to TypeOperation.EQUALS,
    TokenType.LT to TypeOperation.LESSGREATER,
    TokenType.GT to TypeOperation.LESSGREATER,
    TokenType.PLUS to TypeOperation.SUM,
    TokenType.MINUS to TypeOperation.SUM,
    TokenType.SLASH to TypeOperation.PRODUCT,
    TokenType.ASTERISK to TypeOperation.PRODUCT,
    TokenType.LPAREN to TypeOperation.CALL,
    TokenType.LBRACKET to TypeOperation.INDEX,
    TokenType.DOT to TypeOperation.DOT,
)

typealias prefixParseFn = () -> AstExpression
typealias infixParseFn = (AstExpression) -> AstExpression

class Parser(
    val l: Lexer,
    val errors: MutableList<String> = mutableListOf(),

    var curToken: Token? = null,
    var peekToken: Token? = null,

    val prefixParseFns: MutableMap<TokenType, prefixParseFn> = EnumMap(TokenType::class.java),
    val infixParseFns: MutableMap<TokenType, infixParseFn> = EnumMap(TokenType::class.java)
) {
    fun nextToken() {
        curToken = peekToken
        peekToken = l.nextToken()
    }

    fun registerPrefix(tokenType: TokenType, fn: prefixParseFn) {
        prefixParseFns[tokenType] = fn
    }

    fun registerInfix(tokenType: TokenType, fn: infixParseFn) {
        infixParseFns[tokenType] = fn
    }

    fun parseProgram(): AstProgram {

        val program = AstProgram()

        while (curToken?.type != TokenType.EOF) {
            val stmt = parseStatement()
            if (stmt != null) {
                program.statements.add(stmt)
            }
            nextToken()
        }

        return program
    }

    private fun parseStatement(): AstStatement? {
        return when (curToken!!.type) {
            TokenType.LET -> parseLetStatement()
            TokenType.RETURN -> parseReturnStatement()
            else -> parseExpressionStatement()
        }
    }

    private fun parseLetStatement(): AstStatement? {
        val stmt = AstLetStatement(token = curToken!!)

        if (!expectPeek(TokenType.IDENT)) {
            return null
        }

        stmt.name = AstIdentifier(token = curToken!!, value = curToken!!.literal)

        if (!expectPeek(TokenType.ASSIGN)) {
            return null
        }

        nextToken()
        stmt.value = parseExpression(TypeOperation.LOWEST)

        if (peekTokenIs(TokenType.SEMICOLON)) {
            nextToken()
        }

        return stmt
    }

    private fun expectPeek(t: TokenType): Boolean {
        return if (peekTokenIs(t)) {
            nextToken()
            true
        } else {
            peekError(t)
            false
        }
    }

    private fun curTokenIs(t: TokenType): Boolean {
        return curToken?.type == t
    }

    private fun peekTokenIs(t: TokenType): Boolean {
        return peekToken?.type == t
    }

    private fun peekError(t: TokenType) {
        val msg = "expected next token to be %s, got %s instead".format(t, peekToken?.type)
        errors.add(msg)
    }

    private fun parseReturnStatement(): AstStatement {
        val stmt = AstReturnStatement(token = curToken!!)
        nextToken()
        stmt.returnValue = parseExpression(TypeOperation.LOWEST)

        if (peekTokenIs(TokenType.SEMICOLON)) {
            nextToken()
        }
        return stmt
    }

    private fun parseExpressionStatement(): AstStatement {

        val stmt = AstExpressionStatement(token = curToken!!)
        stmt.expression = parseExpression(TypeOperation.LOWEST)

        if (peekTokenIs(TokenType.SEMICOLON)) {
            nextToken()
        }

        return stmt
    }

    //todo посмотреть как это работает
    private fun parseExpression(precedence: TypeOperation): AstExpression? {

        val prefix = prefixParseFns[curToken!!.type]
        if (prefix == null) {
            noPrefixParseFnError(curToken!!.type)
            return null
        }

        var leftExp = prefix()

        while (!peekTokenIs(TokenType.SEMICOLON) && precedence.value < peekPrecedence()) {
            val infix = infixParseFns[peekToken!!.type] ?: return leftExp
            nextToken()
            leftExp = infix(leftExp)
        }
        return leftExp
    }

    private fun peekPrecedence(): Int {
        return precedences[peekToken!!.type]?.value ?: TypeOperation.LOWEST.value
    }

    private fun noPrefixParseFnError(t: TokenType) {
        val msg = "no prefix parse function for %s found".format(t)
        errors.add(msg)
    }
}

fun newParser(l: Lexer): Parser {
    val p = Parser(l = l)

    p.registerPrefix(TokenType.IDENT, p.parseIdentifier)
    p.registerPrefix(TokenType.INT, p.parseIntegerLiteral)
    p.registerPrefix(TokenType.BANG, p.parsePrefixExpression)
    p.registerPrefix(TokenType.MINUS, p.parsePrefixExpression)

    p.registerInfix(TokenType.PLUS, p.parseInfixExpression)
    p.registerInfix(TokenType.MINUS, p.parseInfixExpression)
    p.registerInfix(TokenType.SLASH, p.parseInfixExpression)
    p.registerInfix(TokenType.ASTERISK, p.parseInfixExpression)
    p.registerInfix(TokenType.EQ, p.parseInfixExpression)
    p.registerInfix(TokenType.NOT_EQ, p.parseInfixExpression)
    p.registerInfix(TokenType.LT, p.parseInfixExpression)
    p.registerInfix(TokenType.GT, p.parseInfixExpression)
    p.registerPrefix(TokenType.TRUE, p.parseBoolean)
    p.registerPrefix(TokenType.FALSE, p.parseBoolean)
    p.registerPrefix(TokenType.LPAREN, p.parseGroupedExpression)
    p.registerPrefix(TokenType.IF, p.parseIfExpression)
    p.registerPrefix(TokenType.FUNCTION, p.parseFunctionLiteral)
    p.registerInfix(TokenType.LPAREN, p.parseCallExpression)
    p.registerPrefix(TokenType.STRING, p.parseStringLiteral)
    p.registerPrefix(TokenType.LBRACKET, p.parseArrayLiteral)
    p.registerInfix(TokenType.LBRACKET, p.parseIndexExpression)
    p.registerPrefix(TokenType.LBRACE, p.parseHashLiteral)
    p.registerInfix(TokenType.DOT, p.parseMethodExpression)

    p.nextToken()
    p.nextToken()
    return p
}