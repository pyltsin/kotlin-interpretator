interface AstNode {
    fun tokenLiteral(): String
    fun string(): String
}

interface AstStatement : AstNode {

}

interface AstExpression : AstNode {

}

class AstProgram(val statements: MutableList<AstStatement> = mutableListOf()) : AstNode {
    override fun tokenLiteral(): String {
        return if (statements.isNotEmpty()) {
            statements[0].tokenLiteral()
        } else {
            ""
        }
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        for (statement in statements) {
            builder.append(statement.string())
            builder.append("\n")
        }
        return builder.toString()
    }
}

class AstLetStatement(
    val token: Token,
    var name: AstIdentifier? = null,
    var value: AstExpression? = null,
) : AstStatement {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append(this.tokenLiteral() + " ")
        builder.append(name?.string())
        builder.append(" = ")
        builder.append(value?.string())
        builder.append(";")
        return builder.toString()
    }
}

class AstIdentifier(
    val token: Token,
    val value: String
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        return value
    }
}

class AstReturnStatement(
    val token: Token,
    var returnValue: AstExpression? = null
) : AstStatement {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append(this.tokenLiteral() + " ")
        returnValue?.apply {
            builder.append(this.string())
        }
        builder.append(";")
        return builder.toString()
    }
}

class AstExpressionStatement(
    val token: Token,
    var expression: AstExpression? = null
) : AstStatement {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        return expression?.string() ?: ""
    }
}

class AstIntegerLiteral(
    val token: Token,
    val value: Int
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        return value.toString()
    }
}

class AstStringLiteral(
    val token: Token,
    val value: String
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        return value.toString()
    }
}

class AstBoolean(
    val token: Token,
    val value: Boolean
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        return value.toString()
    }
}

class AstPrefixExpression(
    val token: Token,
    val operator: String,
    var right: AstExpression? = null
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append("(")
        builder.append(this.operator)
        builder.append(this.right?.string())
        builder.append(")")
        return builder.toString()
    }
}

class AstInfixExpression(
    val token: Token,
    val operator: String,
    var right: AstExpression? = null,
    val left: AstExpression,
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append("(")
        builder.append(this.left.string())
        builder.append(" " + this.operator + " ")
        builder.append(this.right?.string())
        builder.append(")")
        return builder.toString()
    }
}

class AstIfExpression(
    val token: Token,
    var condition: AstExpression? = null,
    var consequence: AstBlockStatement? = null,
    var alternative: AstBlockStatement? = null
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append("if")
        builder.append(condition?.string())
        builder.append(" ")
        builder.append(consequence?.string())
        alternative?.apply {
            builder.append("else")
            builder.append(this.string())
        }
        return builder.toString()
    }
}

class AstBlockStatement(
    val token: Token,
    val statements: MutableList<AstStatement> = mutableListOf()
) : AstStatement {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        for (statement in statements) {
            builder.append(statement.string())
        }
        return builder.toString()
    }
}

class AstFunctionLiteral(
    val token: Token,
    var parameters: MutableList<AstIdentifier>? = mutableListOf(),
    var body: AstBlockStatement?=null
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append(this.tokenLiteral())
        builder.append("(")
        builder.append(parameters?.joinToString(separator = ", ") { it.string() })
        builder.append(") ")
        builder.append(body?.string())
        return builder.toString()
    }
}

class AstCallExpression(
    val token: Token,
    val function: AstExpression,
    var arguments: MutableList<AstExpression> = mutableListOf()
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append(this.function.string())
        builder.append("(")
        builder.append(arguments.joinToString(separator = ", ") { it.string() })
        builder.append(") ")
        return builder.toString()
    }
}

class AstArrayLiteral(
    val token: Token,
    var elements: MutableList<AstExpression> = mutableListOf()
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append("[")
        builder.append(elements.joinToString(separator = ", ") { it.string() })
        builder.append("]")
        return builder.toString()
    }
}

class AstIndexExpression(
    val token: Token,
    val left: AstExpression,
    var index: AstExpression? = null
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append("(")
        builder.append(left.string())
        builder.append("[")
        builder.append(index?.string())
        builder.append("])")
        return builder.toString()
    }
}

class AstHashLiteral(
    val token: Token,
    var pairs: MutableMap<AstExpression, AstExpression> = hashMapOf()
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append("{")
        builder.append(pairs.map { it.key.string() + ":" + it.value.string() }.joinToString(separator = ", "))
        builder.append("}")
        return builder.toString()
    }

}

class AstMethodExpression(
    val token: Token,
    var function: AstIdentifier?,
    var arguments: MutableList<AstExpression> = mutableListOf(),
    val self: AstExpression
) : AstExpression {
    override fun tokenLiteral(): String {
        return token.literal
    }

    override fun string(): String {
        val builder: StringBuilder = StringBuilder()
        builder.append(self.string())
        builder.append(".")
        builder.append(function?.string())
        builder.append("(")
        builder.append(arguments.joinToString(separator = ", ") { it.string() })
        builder.append(")")
        return builder.toString()
    }
}