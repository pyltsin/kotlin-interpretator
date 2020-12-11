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

typealias prefixParseFn = () -> AstExpression?
typealias infixParseFn = (AstExpression) -> AstExpression?

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

    private fun parseExpression(precedence: TypeOperation): AstExpression? {

        val prefix = prefixParseFns[curToken!!.type]
        if (prefix == null) {
            noPrefixParseFnError(curToken!!.type)
            return null
        }

        var leftExp: AstExpression? = prefix() ?: return null
        while (!peekTokenIs(TokenType.SEMICOLON) && precedence.value < peekPrecedence()) {
            val infix = infixParseFns[peekToken!!.type] ?: return leftExp
            nextToken()
            leftExp = infix(leftExp!!)
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

    fun parseIdentifier(): AstExpression {
        return AstIdentifier(token = curToken!!, value = curToken!!.literal)
    }

    fun parseIntegerLiteral(): AstExpression? {

        val value = try {
            curToken!!.literal.toInt()
        } catch (e: NumberFormatException) {
            errors.add(String.format("could not parse %q as integer", curToken!!.literal))
            return null
        }
        return AstIntegerLiteral(token = curToken!!, value = value)
    }

    fun parsePrefixExpression(): AstExpression {

        val expression = AstPrefixExpression(
            token = curToken!!,
            operator = curToken!!.literal,
        )
        nextToken()
        expression.right = parseExpression(TypeOperation.PREFIX)
        return expression
    }

    fun parseInfixExpression(left: AstExpression): AstExpression {

        val expression = AstInfixExpression(
            token = curToken!!,
            operator = curToken!!.literal,
            left = left,
        )

        val precedence = curPrecedence()
        nextToken()
        expression.right = parseExpression(precedence)
        return expression
    }

    fun curPrecedence(): TypeOperation {
        return precedences[curToken?.type] ?: TypeOperation.LOWEST
    }

    fun parseStringLiteral(): AstExpression {
        return AstStringLiteral(token = curToken!!, value = curToken!!.literal)
    }

    fun parseGroupedExpression(): AstExpression? {
        nextToken()
        val exp = parseExpression(TypeOperation.LOWEST)
        if (!expectPeek(TokenType.RPAREN)) {
            return null
        }
        return exp
    }

    fun parseBoolean(): AstExpression {
        return AstBoolean(token = curToken!!, value = curTokenIs(TokenType.TRUE))
    }

    fun parseIfExpression(): AstExpression? {
        val expression = AstIfExpression(token = curToken!!)

        if (!expectPeek(TokenType.LPAREN)) {
            return null
        }

        nextToken()

        expression.condition = parseExpression(TypeOperation.LOWEST)

        if (!expectPeek(TokenType.RPAREN)) {
            return null
        }

        if (!expectPeek(TokenType.LBRACE)) {
            return null
        }

        expression.consequence = parseBlockStatement()

        if (peekTokenIs(TokenType.ELSE)) {
            nextToken()
            if (!expectPeek(TokenType.LBRACE)) {
                return null
            }
            expression.alternative = parseBlockStatement()
        }

        return expression
    }

    fun parseBlockStatement(): AstBlockStatement {
        val block = AstBlockStatement(token = curToken!!)

        nextToken()

        while (!curTokenIs(TokenType.RBRACE) && !curTokenIs(TokenType.EOF)) {
            val stmt = parseStatement()
            if (stmt != null) {
                block.statements.add(stmt)
            }
        }
        return block
    }

    fun parseFunctionLiteral(): AstExpression? {
        val lit = AstFunctionLiteral(token = curToken!!)
        if (!expectPeek(TokenType.LPAREN)) {
            return null
        }
        lit.parameters = parseFunctionParameters()
        if (!expectPeek(TokenType.LBRACE)) {
            return null
        }
        lit.body = parseBlockStatement()
        return lit
    }

    private fun parseFunctionParameters(): MutableList<AstIdentifier>? {
        val identifiers = mutableListOf<AstIdentifier>()
        if (peekTokenIs(TokenType.RPAREN)) {
            nextToken()
            return identifiers
        }
        nextToken()
        identifiers.add(AstIdentifier(token = curToken!!, value = curToken!!.literal))
        while (peekTokenIs(TokenType.COMMA)) {
            nextToken()
            nextToken()
            identifiers.add(AstIdentifier(token = curToken!!, value = curToken!!.literal))
        }
        if (!expectPeek(TokenType.RPAREN)) {
            return null
        }
        return identifiers
    }

    fun parseCallExpression(function :AstExpression):AstExpression {
        val exp = AstCallExpression(token = curToken!!, function= function)
        exp.arguments = parseExpressionList(TokenType.RPAREN)!!
        return exp
    }

    fun  parseExpressionList(end : TokenType): MutableList<AstExpression>? {
        val list =  mutableListOf<AstExpression>()
        if (peekTokenIs(end)) {
            nextToken()
            return list
        }
        nextToken()
        list.add(parseExpression(TypeOperation.LOWEST)!!)
        while (peekTokenIs(TokenType.COMMA)) {
            nextToken()
            nextToken()
            list.add(parseExpression(TypeOperation.LOWEST)!!)
        }
        if (!expectPeek(end)) {
            return null
        }
        return list
    }

    fun  parseArrayLiteral() : AstExpression {
        val array = AstArrayLiteral(token = curToken!!)
        array.elements = parseExpressionList(TokenType.RBRACKET)!!
        return array
    }

    fun  parseIndexExpression(left :AstExpression) : AstExpression? {
        val exp = AstIndexExpression(token = curToken!!, left= left)
        nextToken()
        exp.index = parseExpression(TypeOperation.LOWEST)
        if (!expectPeek(TokenType.RBRACKET)) {
            return null
        }
        return exp
    }

    fun  parseHashLiteral(): AstExpression? {
        val hash = AstHashLiteral(token = curToken!!)
        hash.pairs = mutableMapOf()
        while (!peekTokenIs(TokenType.RBRACE)) {
            nextToken()
            val key: AstExpression? = parseExpression(TypeOperation.LOWEST)
            if (!expectPeek(TokenType.COLON)) {
                return null
            }
            nextToken()
            val value = parseExpression(TypeOperation.LOWEST)
            hash.pairs[key!!] = value!!
            if (!peekTokenIs(TokenType.RBRACE) && !expectPeek(TokenType.COMMA)) {
                return null
            }
        }
        if (!expectPeek(TokenType.RBRACE)) {
            return null
        }
        return hash
    }
    fun  parseMethodExpression(self: AstExpression) : AstExpression? {

        val expression = AstMethodExpression(
                token = curToken!!,
                self =      self,
                arguments = mutableListOf(),
                function = null,
        )

        if (!expectPeek(TokenType.IDENT)) {
            return null
        }
        expression.function = AstIdentifier(token = curToken!!, value = curToken!!.literal)

        nextToken()
        expression.arguments = parseExpressionList(TokenType.RPAREN)!!

        return expression
    }
}

fun newParser(l: Lexer): Parser {
    val p = Parser(l = l)

    p.registerPrefix(TokenType.IDENT) { p.parseIdentifier() }
    p.registerPrefix(TokenType.INT) { p.parseIntegerLiteral() }
    p.registerPrefix(TokenType.BANG) { p.parsePrefixExpression() }
    p.registerPrefix(TokenType.MINUS) { p.parsePrefixExpression() }

    p.registerPrefix(TokenType.STRING) { p.parseStringLiteral() }
    p.registerPrefix(TokenType.LPAREN) { p.parseGroupedExpression() }
    p.registerPrefix(TokenType.TRUE) { p.parseBoolean() }
    p.registerPrefix(TokenType.FALSE) { p.parseBoolean() }
    p.registerPrefix(TokenType.IF) { p.parseIfExpression() }
    p.registerPrefix(TokenType.FUNCTION) { p.parseFunctionLiteral() }
    p.registerPrefix(TokenType.LBRACKET) { p.parseArrayLiteral() }
    p.registerPrefix(TokenType.LBRACE) { p.parseHashLiteral() }

    p.registerInfix(TokenType.PLUS) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.MINUS) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.SLASH) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.ASTERISK) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.EQ) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.NOT_EQ) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.LT) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.GT) { p.parseInfixExpression(it) }
    p.registerInfix(TokenType.LPAREN) { p.parseCallExpression(it) }
    p.registerInfix(TokenType.LBRACKET) {  p.parseIndexExpression(it) }
    p.registerInfix(TokenType.DOT) {  p.parseMethodExpression(it) }

    p.nextToken()
    p.nextToken()
    return p
}