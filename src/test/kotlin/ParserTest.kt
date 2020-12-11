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

    @Test
    fun testReturnStatements() {
        val testCases = listOf(
            Pair<String, Any>("return 5;", 5),
            Pair<String, Any>("return true;", true),
            Pair<String, Any>("return foobar;", "foobar"),
        )
        for (testCase in testCases) {
            val lexer = newLexer(testCase.first)
            val parser = newParser(lexer)
            val program = parser.parseProgram()

            checkParseErrors(parser)

            assertEquals(1, program.statements.size)
            val astStatement: AstStatement = program.statements[0]
            assertTrue(astStatement is AstReturnStatement)
            assertEquals("return", astStatement.tokenLiteral())
            testLiteralExpression(astStatement.returnValue, testCase.second)
        }
    }

    @Test
    fun testParsingPrefixExpressions() {
        val testCases = listOf(
            Triple<String, String, Any>("!5;", "!", 5),
            Triple<String, String, Any>("-15;", "-", 15),
            Triple<String, String, Any>("!foobar;", "!", "foobar"),
            Triple<String, String, Any>("-foobar;", "-", "foobar"),
            Triple<String, String, Any>("!true;", "!", true),
            Triple<String, String, Any>("!false;", "!", false),
        )

        for (testCase in testCases) {
            val lexer = newLexer(testCase.first)
            val parser = newParser(lexer)
            val program = parser.parseProgram()

            checkParseErrors(parser)

            assertEquals(1, program.statements.size)
            val astStatement: AstStatement = program.statements[0]

            assertTrue(astStatement is AstExpressionStatement)
            val expression = astStatement.expression
            assertTrue(expression is AstPrefixExpression)
            assertEquals(testCase.second, expression.operator)
            testLiteralExpression(expression.right, testCase.third)

        }
    }

    data class Fourth<out A, out B, out C, out D>(
        val first: A,
        val second: B,
        val third: C,
        val fourth: D,
    )

    @Test
    fun testParsingInfixExpressions() {
        val testCases = listOf(
            Fourth<String, Any, String, Any>("5 + 5;", 5, "+", 5),
            Fourth<String, Any, String, Any>("5 - 5;", 5, "-", 5),
            Fourth<String, Any, String, Any>("5 * 5;", 5, "*", 5),
            Fourth<String, Any, String, Any>("5 / 5;", 5, "/", 5),
            Fourth<String, Any, String, Any>("5 > 5;", 5, ">", 5),
            Fourth<String, Any, String, Any>("5 < 5;", 5, "<", 5),
            Fourth<String, Any, String, Any>("5 == 5;", 5, "==", 5),
            Fourth<String, Any, String, Any>("5 != 5;", 5, "!=", 5),
            Fourth<String, Any, String, Any>("foobar + barfoo;", "foobar", "+", "barfoo"),
            Fourth<String, Any, String, Any>("true == true", true, "==", true),
            Fourth<String, Any, String, Any>("false == false", false, "==", false),
        )

        for (testCase in testCases) {
            val lexer = newLexer(testCase.first)
            val parser = newParser(lexer)
            val program = parser.parseProgram()

            checkParseErrors(parser)

            assertEquals(1, program.statements.size)

            val astStatement: AstStatement = program.statements[0]

            assertTrue(astStatement is AstExpressionStatement)

            testInfixExpression(testCase, astStatement.expression)
        }
    }

    @Test
    fun testOperatorPrecedenceParsing() {
        val testCases = listOf(
            Pair<String, String>(
                "-a * b",
                "((-a) * b)",
            ),
            Pair<String, String>(
                "!-a",
                "(!(-a))",
            ),
            Pair<String, String>(
                "a + b + c",
                "((a + b) + c)",
            ),
            Pair<String, String>(
                "a + b - c",
                "((a + b) - c)",
            ),
            Pair<String, String>(
                "a * b * c",
                "((a * b) * c)",
            ),
            Pair<String, String>(
                "a * b / c",
                "((a * b) / c)",
            ),
            Pair<String, String>(
                "a + b / c",
                "(a + (b / c))",
            ),
            Pair<String, String>(
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f)",
            ),
            Pair<String, String>(
                "3 + 4; -5 * 5",
                "(3 + 4)((-5) * 5)",
            ),
            Pair<String, String>(
                "5 > 4 == 3 < 4",
                "((5 > 4) == (3 < 4))",
            ),
            Pair<String, String>(
                "5 < 4 != 3 > 4",
                "((5 < 4) != (3 > 4))",
            ),
            Pair<String, String>(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            Pair<String, String>(
                "true",
                "true",
            ),
            Pair<String, String>(
                "false",
                "false",
            ),
            Pair<String, String>(
                "3 > 5 == false",
                "((3 > 5) == false)",
            ),
            Pair<String, String>(
                "3 < 5 == true",
                "((3 < 5) == true)",
            ),
            Pair<String, String>(
                "1 + (2 + 3) + 4",
                "((1 + (2 + 3)) + 4)",
            ),
            Pair<String, String>(
                "(5 + 5) * 2",
                "((5 + 5) * 2)",
            ),
            Pair<String, String>(
                "2 / (5 + 5)",
                "(2 / (5 + 5))",
            ),
            Pair<String, String>(
                "(5 + 5) * 2 * (5 + 5)",
                "(((5 + 5) * 2) * (5 + 5))",
            ),
            Pair<String, String>(
                "-(5 + 5)",
                "(-(5 + 5))",
            ),
            Pair<String, String>(
                "!(true == true)",
                "(!(true == true))",
            ),
            Pair<String, String>(
                "a + add(b * c) + d",
                "((a + add((b * c))) + d)",
            ),
            Pair<String, String>(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            Pair<String, String>(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            Pair<String, String>(
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            Pair<String, String>(
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        )
        for (testCase in testCases) {
            val lexer = newLexer(testCase.first)
            val parser = newParser(lexer)
            val program = parser.parseProgram()

            checkParseErrors(parser)
            assertEquals(program.string().trim().replace("\n", "")
                .replace(" ", ""), testCase.second.replace(" ", ""))
        }
    }

//    func TestIfElseExpression(t *testing.T) {
//        input := `if (x < y) { x } else { y }`
//
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//
//        if len(program.Statements) != 1 {
//            t.Fatalf("program.Body does not contain %d statements. got=%d\n",
//                1, len(program.Statements))
//        }
//
//        stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
//        if !ok {
//            t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
//                program.Statements[0])
//        }
//
//        exp, ok := stmt.Expression.(*ast.IfExpression)
//        if !ok {
//            t.Fatalf("stmt.Expression is not ast.IfExpression. got=%T", stmt.Expression)
//        }
//
//        if !testInfixExpression(t, exp.Condition, "x", "<", "y") {
//            return
//        }
//
//        if len(exp.Consequence.Statements) != 1 {
//            t.Errorf("consequence is not 1 statements. got=%d\n",
//                len(exp.Consequence.Statements))
//        }
//
//        consequence, ok := exp.Consequence.Statements[0].(*ast.ExpressionStatement)
//        if !ok {
//            t.Fatalf("Statements[0] is not ast.ExpressionStatement. got=%T",
//                exp.Consequence.Statements[0])
//        }
//
//        if !testIdentifier(t, consequence.Expression, "x") {
//            return
//        }
//
//        if len(exp.Alternative.Statements) != 1 {
//            t.Errorf("exp.Alternative.Statements does not contain 1 statements. got=%d\n",
//                len(exp.Alternative.Statements))
//        }
//
//        alternative, ok := exp.Alternative.Statements[0].(*ast.ExpressionStatement)
//        if !ok {
//            t.Fatalf("Statements[0] is not ast.ExpressionStatement. got=%T",
//                exp.Alternative.Statements[0])
//        }
//
//        if !testIdentifier(t, alternative.Expression, "y") {
//            return
//        }
//    }
//
//    func TestFunctionLiteralParsing(t *testing.T) {
//        input := `fn(x, y) { x + y; }`
//
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//
//        if len(program.Statements) != 1 {
//            t.Fatalf("program.Body does not contain %d statements. got=%d\n",
//                1, len(program.Statements))
//        }
//
//        stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
//        if !ok {
//            t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
//                program.Statements[0])
//        }
//
//        function, ok := stmt.Expression.(*ast.FunctionLiteral)
//        if !ok {
//            t.Fatalf("stmt.Expression is not ast.FunctionLiteral. got=%T",
//                stmt.Expression)
//        }
//
//        if len(function.Parameters) != 2 {
//            t.Fatalf("function literal parameters wrong. want 2, got=%d\n",
//                len(function.Parameters))
//        }
//
//        testLiteralExpression(t, function.Parameters[0], "x")
//        testLiteralExpression(t, function.Parameters[1], "y")
//
//        if len(function.Body.Statements) != 1 {
//            t.Fatalf("function.Body.Statements has not 1 statements. got=%d\n",
//                len(function.Body.Statements))
//        }
//
//        bodyStmt, ok := function.Body.Statements[0].(*ast.ExpressionStatement)
//        if !ok {
//            t.Fatalf("function body stmt is not ast.ExpressionStatement. got=%T",
//                function.Body.Statements[0])
//        }
//
//        testInfixExpression(t, bodyStmt.Expression, "x", "+", "y")
//    }
//
//    func TestFunctionParameterParsing(t *testing.T) {
//        tests := []struct {
//            input          string
//                    expectedParams []string
//        }{
//            {input: "fn() {};", expectedParams: []string{}},
//            {input: "fn(x) {};", expectedParams: []string{"x"}},
//            {input: "fn(x, y, z) {};", expectedParams: []string{"x", "y", "z"}},
//        }
//
//        for _, tt := range tests {
//            l := lexer.New(tt.input)
//            p := New(l)
//            program := p.ParseProgram()
//            checkParserErrors(t, p)
//
//            stmt := program.Statements[0].(*ast.ExpressionStatement)
//            function := stmt.Expression.(*ast.FunctionLiteral)
//
//            if len(function.Parameters) != len(tt.expectedParams) {
//                t.Errorf("length parameters wrong. want %d, got=%d\n",
//                    len(tt.expectedParams), len(function.Parameters))
//            }
//
//            for i, ident := range tt.expectedParams {
//            testLiteralExpression(t, function.Parameters[i], ident)
//        }
//        }
//    }
//
//    func TestCallExpressionParsing(t *testing.T) {
//        input := "add(1, 2 * 3, 4 + 5);"
//
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//
//        if len(program.Statements) != 1 {
//            t.Fatalf("program.Statements does not contain %d statements. got=%d\n",
//                1, len(program.Statements))
//        }
//
//        stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
//        if !ok {
//            t.Fatalf("stmt is not ast.ExpressionStatement. got=%T",
//                program.Statements[0])
//        }
//
//        exp, ok := stmt.Expression.(*ast.CallExpression)
//        if !ok {
//            t.Fatalf("stmt.Expression is not ast.CallExpression. got=%T",
//                stmt.Expression)
//        }
//
//        if !testIdentifier(t, exp.Function, "add") {
//            return
//        }
//
//        if len(exp.Arguments) != 3 {
//            t.Fatalf("wrong length of arguments. got=%d", len(exp.Arguments))
//        }
//
//        testLiteralExpression(t, exp.Arguments[0], 1)
//        testInfixExpression(t, exp.Arguments[1], 2, "*", 3)
//        testInfixExpression(t, exp.Arguments[2], 4, "+", 5)
//    }
//
//    func TestCallExpressionParameterParsing(t *testing.T) {
//        tests := []struct {
//            input         string
//                    expectedIdent string
//                    expectedArgs  []string
//        }{
//            {
//                input:         "add();",
//                expectedIdent: "add",
//                expectedArgs:  []string{},
//            },
//            {
//                input:         "add(1);",
//                expectedIdent: "add",
//                expectedArgs:  []string{"1"},
//            },
//            {
//                input:         "add(1, 2 * 3, 4 + 5);",
//                expectedIdent: "add",
//                expectedArgs:  []string{"1", "(2 * 3)", "(4 + 5)"},
//            },
//        }
//
//        for _, tt := range tests {
//            l := lexer.New(tt.input)
//            p := New(l)
//            program := p.ParseProgram()
//            checkParserErrors(t, p)
//
//            stmt := program.Statements[0].(*ast.ExpressionStatement)
//            exp, ok := stmt.Expression.(*ast.CallExpression)
//            if !ok {
//                t.Fatalf("stmt.Expression is not ast.CallExpression. got=%T",
//                    stmt.Expression)
//            }
//
//            if !testIdentifier(t, exp.Function, tt.expectedIdent) {
//                return
//            }
//
//            if len(exp.Arguments) != len(tt.expectedArgs) {
//                t.Fatalf("wrong number of arguments. want=%d, got=%d",
//                    len(tt.expectedArgs), len(exp.Arguments))
//            }
//
//            for i, arg := range tt.expectedArgs {
//            if exp.Arguments[i].String() != arg {
//                t.Errorf("argument %d wrong. want=%q, got=%q", i,
//                    arg, exp.Arguments[i].String())
//            }
//        }
//        }
//    }
//    func TestParsingArrayLiterals(t *testing.T) {
//        input := "[1, 2 * 2, 3 + 3]"
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//        stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
//        array, ok := stmt.Expression.(*ast.ArrayLiteral)
//        if !ok {
//            t.Fatalf("exp not ast.ArrayLiteral. got=%T", stmt.Expression)
//        }
//        if len(array.Elements) != 3 {
//            t.Fatalf("len(array.Elements) not 3. got=%d", len(array.Elements))
//        }
//        testIntegerLiteral(t, array.Elements[0], 1)
//        testInfixExpression(t, array.Elements[1], 2, "*", 2)
//        testInfixExpression(t, array.Elements[2], 3, "+", 3)
//    }
//
//    func TestParsingIndexExpressions(t *testing.T) {
//        input := "myArray[1 + 1]"
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//        stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
//        indexExp, ok := stmt.Expression.(*ast.IndexExpression)
//        if !ok {
//            t.Fatalf("exp not *ast.IndexExpression. got=%T", stmt.Expression)
//        }
//        if !testIdentifier(t, indexExp.Left, "myArray") {
//            return
//        }
//        if !testInfixExpression(t, indexExp.Index, 1, "+", 1) {
//            return
//        }
//    }
//
//    func TestParsingHashLiteralsStringKeys(t *testing.T) {
//        input := `{"one": 1, "two": 2, "three": 3}`
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//        stmt := program.Statements[0].(*ast.ExpressionStatement)
//        hash, ok := stmt.Expression.(*ast.HashLiteral)
//        if !ok {
//            t.Fatalf("exp is not ast.HashLiteral. got=%T", stmt.Expression)
//        }
//        if len(hash.Pairs) != 3 {
//            t.Errorf("hash.Pairs has wrong length. got=%d", len(hash.Pairs))
//        }
//        expected := map[string]int64{
//            "one":   1,
//            "two":   2,
//            "three": 3,
//        }
//        for key, value := range hash.Pairs {
//            literal, ok := key.(*ast.StringLiteral)
//            if !ok {
//                t.Errorf("key is not ast.StringLiteral. got=%T", key)
//            }
//            expectedValue := expected[literal.String()]
//            testIntegerLiteral(t, value, expectedValue)
//        }
//    }
//
//    func TestParsingEmptyHashLiteral(t *testing.T) {
//        input := "{}"
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//        stmt := program.Statements[0].(*ast.ExpressionStatement)
//        hash, ok := stmt.Expression.(*ast.HashLiteral)
//        if !ok {
//            t.Fatalf("exp is not ast.HashLiteral. got=%T", stmt.Expression)
//        }
//        if len(hash.Pairs) != 0 {
//            t.Errorf("hash.Pairs has wrong length. got=%d", len(hash.Pairs))
//        }
//    }
//
//    func TestMethod(t *testing.T) {
//        input := "a.method(1, 1+2)"
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//        if len(program.Statements) != 1 {
//            t.Errorf("many parse")
//        }
//        stmt := program.Statements[0].(*ast.ExpressionStatement)
//        method, ok := stmt.Expression.(*ast.MethodExpression)
//        if !ok {
//            t.Fatalf("exp is not ast.MethodExpression. got=%T", stmt.Expression)
//        }
//
//        if method.String() != "a.method(1, (1 + 2))" {
//            t.Fatalf("exp is a.method(1, (1 + 2)). got=%T", method.String())
//        }
//    }
//
//    func TestParsingHashLiteralsWithExpressions(t *testing.T) {
//        input := `{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}`
//        l := lexer.New(input)
//        p := New(l)
//        program := p.ParseProgram()
//        checkParserErrors(t, p)
//        stmt := program.Statements[0].(*ast.ExpressionStatement)
//        hash, ok := stmt.Expression.(*ast.HashLiteral)
//        if !ok {
//            t.Fatalf("exp is not ast.HashLiteral. got=%T", stmt.Expression)
//        }
//        if len(hash.Pairs) != 3 {
//            t.Errorf("hash.Pairs has wrong length. got=%d", len(hash.Pairs))
//        }
//        tests := map[string]func(ast.Expression){
//            "one": func(e ast.Expression) {
//            testInfixExpression(t, e, 0, "+", 1)
//        },
//            "two": func(e ast.Expression) {
//            testInfixExpression(t, e, 10, "-", 8)
//        },
//            "three": func(e ast.Expression) {
//            testInfixExpression(t, e, 15, "/", 5)
//        },
//        }
//        for key, value := range hash.Pairs {
//            literal, ok := key.(*ast.StringLiteral)
//            if !ok {
//                t.Errorf("key is not ast.StringLiteral. got=%T", key)
//                continue
//            }
//            testFunc, ok := tests[literal.String()]
//            if !ok {
//                t.Errorf("No test function for key %q found", literal.String())
//                continue
//            }
//            testFunc(value)
//        }
//    }

    private fun testInfixExpression(testCase: Fourth<String, Any, String, Any>, expression: AstExpression?) {
        assertNotNull(expression)
        assertTrue(expression is AstInfixExpression)
        testLiteralExpression(expression.left, testCase.second)
        testLiteralExpression(expression.right, testCase.fourth)
        assertEquals(expression.operator, testCase.third)
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
        assertEquals(name, s.name?.value)
        assertEquals(name, s.name?.tokenLiteral())
    }

    private fun testLiteralExpression(exp: AstExpression?, expected: Any) {
        assertNotNull(exp)
        when (expected) {
            is Int -> testIntegerLiteral(exp, expected)
            is String -> testIdentifier(exp, expected)
            is Boolean -> testBooleanLiteral(exp, expected)
        }
    }

    private fun testIntegerLiteral(il: AstExpression, value: Int) {
        assertTrue(il is AstIntegerLiteral)
        assertEquals(il.value, value)
        assertEquals(il.tokenLiteral(), value.toString())
    }

    private fun testIdentifier(il: AstExpression, value: String) {
        assertTrue(il is AstIdentifier)
        assertEquals(il.value, value)
        assertEquals(il.tokenLiteral(), value)
    }

    private fun testBooleanLiteral(il: AstExpression, value: Boolean) {
        assertTrue(il is AstBoolean)
        assertEquals(il.value, value)
        assertEquals(il.tokenLiteral(), value.toString())
    }
}