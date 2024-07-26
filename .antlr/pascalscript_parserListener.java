// Generated from /workspaces/pascalscript/pascalscript_parser.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link pascalscript_parserParser}.
 */
public interface pascalscript_parserListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#pascalFile}.
	 * @param ctx the parse tree
	 */
	void enterPascalFile(pascalscript_parserParser.PascalFileContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#pascalFile}.
	 * @param ctx the parse tree
	 */
	void exitPascalFile(pascalscript_parserParser.PascalFileContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#pascalProgram}.
	 * @param ctx the parse tree
	 */
	void enterPascalProgram(pascalscript_parserParser.PascalProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#pascalProgram}.
	 * @param ctx the parse tree
	 */
	void exitPascalProgram(pascalscript_parserParser.PascalProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#pascalHeader}.
	 * @param ctx the parse tree
	 */
	void enterPascalHeader(pascalscript_parserParser.PascalHeaderContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#pascalHeader}.
	 * @param ctx the parse tree
	 */
	void exitPascalHeader(pascalscript_parserParser.PascalHeaderContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#pascalBlock}.
	 * @param ctx the parse tree
	 */
	void enterPascalBlock(pascalscript_parserParser.PascalBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#pascalBlock}.
	 * @param ctx the parse tree
	 */
	void exitPascalBlock(pascalscript_parserParser.PascalBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#instructionBlock}.
	 * @param ctx the parse tree
	 */
	void enterInstructionBlock(pascalscript_parserParser.InstructionBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#instructionBlock}.
	 * @param ctx the parse tree
	 */
	void exitInstructionBlock(pascalscript_parserParser.InstructionBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#instruction}.
	 * @param ctx the parse tree
	 */
	void enterInstruction(pascalscript_parserParser.InstructionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#instruction}.
	 * @param ctx the parse tree
	 */
	void exitInstruction(pascalscript_parserParser.InstructionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#assignment}.
	 * @param ctx the parse tree
	 */
	void enterAssignment(pascalscript_parserParser.AssignmentContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#assignment}.
	 * @param ctx the parse tree
	 */
	void exitAssignment(pascalscript_parserParser.AssignmentContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#constBlock}.
	 * @param ctx the parse tree
	 */
	void enterConstBlock(pascalscript_parserParser.ConstBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#constBlock}.
	 * @param ctx the parse tree
	 */
	void exitConstBlock(pascalscript_parserParser.ConstBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#constantDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterConstantDeclaration(pascalscript_parserParser.ConstantDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#constantDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitConstantDeclaration(pascalscript_parserParser.ConstantDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#constantValue}.
	 * @param ctx the parse tree
	 */
	void enterConstantValue(pascalscript_parserParser.ConstantValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#constantValue}.
	 * @param ctx the parse tree
	 */
	void exitConstantValue(pascalscript_parserParser.ConstantValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#typeReference}.
	 * @param ctx the parse tree
	 */
	void enterTypeReference(pascalscript_parserParser.TypeReferenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#typeReference}.
	 * @param ctx the parse tree
	 */
	void exitTypeReference(pascalscript_parserParser.TypeReferenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#scalarType}.
	 * @param ctx the parse tree
	 */
	void enterScalarType(pascalscript_parserParser.ScalarTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#scalarType}.
	 * @param ctx the parse tree
	 */
	void exitScalarType(pascalscript_parserParser.ScalarTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#varBlock}.
	 * @param ctx the parse tree
	 */
	void enterVarBlock(pascalscript_parserParser.VarBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#varBlock}.
	 * @param ctx the parse tree
	 */
	void exitVarBlock(pascalscript_parserParser.VarBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#variableDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterVariableDeclaration(pascalscript_parserParser.VariableDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#variableDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitVariableDeclaration(pascalscript_parserParser.VariableDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#identifierList}.
	 * @param ctx the parse tree
	 */
	void enterIdentifierList(pascalscript_parserParser.IdentifierListContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#identifierList}.
	 * @param ctx the parse tree
	 */
	void exitIdentifierList(pascalscript_parserParser.IdentifierListContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterExpression(pascalscript_parserParser.ExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitExpression(pascalscript_parserParser.ExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#unaryOperator}.
	 * @param ctx the parse tree
	 */
	void enterUnaryOperator(pascalscript_parserParser.UnaryOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#unaryOperator}.
	 * @param ctx the parse tree
	 */
	void exitUnaryOperator(pascalscript_parserParser.UnaryOperatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#relationalOperator}.
	 * @param ctx the parse tree
	 */
	void enterRelationalOperator(pascalscript_parserParser.RelationalOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#relationalOperator}.
	 * @param ctx the parse tree
	 */
	void exitRelationalOperator(pascalscript_parserParser.RelationalOperatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#additiveOperator}.
	 * @param ctx the parse tree
	 */
	void enterAdditiveOperator(pascalscript_parserParser.AdditiveOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#additiveOperator}.
	 * @param ctx the parse tree
	 */
	void exitAdditiveOperator(pascalscript_parserParser.AdditiveOperatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#multiplicativeOperator}.
	 * @param ctx the parse tree
	 */
	void enterMultiplicativeOperator(pascalscript_parserParser.MultiplicativeOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#multiplicativeOperator}.
	 * @param ctx the parse tree
	 */
	void exitMultiplicativeOperator(pascalscript_parserParser.MultiplicativeOperatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#variableReference}.
	 * @param ctx the parse tree
	 */
	void enterVariableReference(pascalscript_parserParser.VariableReferenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#variableReference}.
	 * @param ctx the parse tree
	 */
	void exitVariableReference(pascalscript_parserParser.VariableReferenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link pascalscript_parserParser#constantReference}.
	 * @param ctx the parse tree
	 */
	void enterConstantReference(pascalscript_parserParser.ConstantReferenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link pascalscript_parserParser#constantReference}.
	 * @param ctx the parse tree
	 */
	void exitConstantReference(pascalscript_parserParser.ConstantReferenceContext ctx);
}