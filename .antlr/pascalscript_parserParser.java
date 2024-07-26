// Generated from /workspaces/pascalscript/pascalscript_parser.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class pascalscript_parserParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		WS=1, PROGRAM=2, BEGIN=3, END=4, IF=5, THEN=6, ELSE=7, REPEAT=8, UNTIL=9, 
		WHILE=10, DO=11, FOR=12, TO=13, DOWNTO=14, WRITE=15, WRITELN=16, FALSE=17, 
		TRUE=18, LABEL=19, GOTO=20, INTEGER=21, CARDINAL=22, BOOLEAN=23, CHAR=24, 
		STRING=25, REAL=26, ARRAY=27, OF=28, RECORD=29, CONST=30, TYPE=31, VAR=32, 
		PROCEDURE=33, FUNCTION=34, NOT=35, OR=36, XOR=37, DIV=38, MOD=39, AND=40, 
		SHL=41, SHR=42, PLUS=43, MINUS=44, STAR=45, SLASH=46, EQUAL=47, NOT_EQUAL=48, 
		LESS_THAN=49, LESS_OR_EQUAL=50, GREATER_THAN=51, GREATER_OR_EQUAL=52, 
		DOT_COLON=53, AT_SIGN=54, CARET=55, COLON=56, COMMA=57, DOT_DOT=58, DOT=59, 
		LEFT_BRACKET=60, LEFT_PARENTHESIS=61, RIGHT_BRACKET=62, RIGHT_PARENTHESIS=63, 
		SEMI_COLON=64, UNDERSCORE=65, QUOTE=66, PERCENT=67, AMPERSAND=68, DOLLAR=69, 
		IDENTIFIER=70, IDENTIFIER_PREFIX=71, LETTER=72, UNSIGNED_INTEGER_VALUE=73, 
		UNSIGNED_REAL_VALUE=74, DECIMAL_DIGIT_SEQUENCE=75, BINARY_DIGIT_SEQUENCE=76, 
		OCTAL_DIGIT_SEQUENCE=77, HEXADECIMAL_DIGIT_SEQUENCE=78, SIGN=79, DECIMAL_DIGIT=80, 
		BINARY_DIGIT=81, OCTAL_DIGIT=82, HEXADECIMAL_DIGIT=83, CHARACTER_VALUE=84, 
		COMPOSED_STRING_VALUE=85, CHAR_VALUE=86, CONTROL_CHAR=87, QUOTED_CHAR=88, 
		QUOTED_STRING=89, BOOLEAN_VALUE=90, ANY_CHAR=91, COMMENT1=92, COMMENT2=93, 
		COMMENT3=94;
	public static final int
		RULE_pascalFile = 0, RULE_pascalProgram = 1, RULE_pascalHeader = 2, RULE_pascalBlock = 3, 
		RULE_instructionBlock = 4, RULE_instruction = 5, RULE_assignment = 6, 
		RULE_procedureCall = 7, RULE_parameterList = 8, RULE_parameter = 9, RULE_constBlock = 10, 
		RULE_constantDeclaration = 11, RULE_typeReference = 12, RULE_scalarType = 13, 
		RULE_varBlock = 14, RULE_variableDeclaration = 15, RULE_identifierList = 16, 
		RULE_expression = 17, RULE_unaryOperator = 18, RULE_relationalOperator = 19, 
		RULE_additiveOperator = 20, RULE_multiplicativeOperator = 21, RULE_variableReference = 22, 
		RULE_constantReference = 23;
	private static String[] makeRuleNames() {
		return new String[] {
			"pascalFile", "pascalProgram", "pascalHeader", "pascalBlock", "instructionBlock", 
			"instruction", "assignment", "procedureCall", "parameterList", "parameter", 
			"constBlock", "constantDeclaration", "typeReference", "scalarType", "varBlock", 
			"variableDeclaration", "identifierList", "expression", "unaryOperator", 
			"relationalOperator", "additiveOperator", "multiplicativeOperator", "variableReference", 
			"constantReference"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, null, "'PROGRAM'", "'BEGIN'", "'END'", "'IF'", "'THEN'", "'ELSE'", 
			"'REPEAT'", "'UNTIL'", "'WHILE'", "'DO'", "'FOR'", "'TO'", "'DOWNTO'", 
			"'WRITE'", "'WRITELN'", "'FALSE'", "'TRUE'", "'LABEL'", "'GOTO'", "'INTEGER'", 
			"'CARDINAL'", "'BOOLEAN'", "'CHAR'", "'STRING'", "'REAL'", "'ARRAY'", 
			"'OF'", "'RECORD'", "'CONST'", "'TYPE'", "'VAR'", "'PROCEDURE'", "'FUNCTION'", 
			"'NOT'", "'OR'", "'XOR'", "'DIV'", "'MOD'", "'AND'", "'SHL'", "'SHR'", 
			"'+'", "'-'", "'*'", "'/'", "'='", "'<>'", "'<'", "'<='", "'>'", "'>='", 
			"':='", "'@'", "'^'", "':'", "','", "'..'", "'.'", "'['", "'('", "']'", 
			"')'", "';'", "'_'", "'''", "'%'", "'&'", "'$'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "WS", "PROGRAM", "BEGIN", "END", "IF", "THEN", "ELSE", "REPEAT", 
			"UNTIL", "WHILE", "DO", "FOR", "TO", "DOWNTO", "WRITE", "WRITELN", "FALSE", 
			"TRUE", "LABEL", "GOTO", "INTEGER", "CARDINAL", "BOOLEAN", "CHAR", "STRING", 
			"REAL", "ARRAY", "OF", "RECORD", "CONST", "TYPE", "VAR", "PROCEDURE", 
			"FUNCTION", "NOT", "OR", "XOR", "DIV", "MOD", "AND", "SHL", "SHR", "PLUS", 
			"MINUS", "STAR", "SLASH", "EQUAL", "NOT_EQUAL", "LESS_THAN", "LESS_OR_EQUAL", 
			"GREATER_THAN", "GREATER_OR_EQUAL", "DOT_COLON", "AT_SIGN", "CARET", 
			"COLON", "COMMA", "DOT_DOT", "DOT", "LEFT_BRACKET", "LEFT_PARENTHESIS", 
			"RIGHT_BRACKET", "RIGHT_PARENTHESIS", "SEMI_COLON", "UNDERSCORE", "QUOTE", 
			"PERCENT", "AMPERSAND", "DOLLAR", "IDENTIFIER", "IDENTIFIER_PREFIX", 
			"LETTER", "UNSIGNED_INTEGER_VALUE", "UNSIGNED_REAL_VALUE", "DECIMAL_DIGIT_SEQUENCE", 
			"BINARY_DIGIT_SEQUENCE", "OCTAL_DIGIT_SEQUENCE", "HEXADECIMAL_DIGIT_SEQUENCE", 
			"SIGN", "DECIMAL_DIGIT", "BINARY_DIGIT", "OCTAL_DIGIT", "HEXADECIMAL_DIGIT", 
			"CHARACTER_VALUE", "COMPOSED_STRING_VALUE", "CHAR_VALUE", "CONTROL_CHAR", 
			"QUOTED_CHAR", "QUOTED_STRING", "BOOLEAN_VALUE", "ANY_CHAR", "COMMENT1", 
			"COMMENT2", "COMMENT3"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "pascalscript_parser.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public pascalscript_parserParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PascalFileContext extends ParserRuleContext {
		public PascalProgramContext pascalProgram() {
			return getRuleContext(PascalProgramContext.class,0);
		}
		public PascalFileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pascalFile; }
	}

	public final PascalFileContext pascalFile() throws RecognitionException {
		PascalFileContext _localctx = new PascalFileContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_pascalFile);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(48);
			pascalProgram();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PascalProgramContext extends ParserRuleContext {
		public TerminalNode PROGRAM() { return getToken(pascalscript_parserParser.PROGRAM, 0); }
		public TerminalNode IDENTIFIER() { return getToken(pascalscript_parserParser.IDENTIFIER, 0); }
		public TerminalNode SEMI_COLON() { return getToken(pascalscript_parserParser.SEMI_COLON, 0); }
		public PascalHeaderContext pascalHeader() {
			return getRuleContext(PascalHeaderContext.class,0);
		}
		public InstructionBlockContext instructionBlock() {
			return getRuleContext(InstructionBlockContext.class,0);
		}
		public TerminalNode DOT() { return getToken(pascalscript_parserParser.DOT, 0); }
		public TerminalNode EOF() { return getToken(pascalscript_parserParser.EOF, 0); }
		public PascalProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pascalProgram; }
	}

	public final PascalProgramContext pascalProgram() throws RecognitionException {
		PascalProgramContext _localctx = new PascalProgramContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_pascalProgram);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(50);
			match(PROGRAM);
			setState(51);
			match(IDENTIFIER);
			setState(52);
			match(SEMI_COLON);
			setState(53);
			pascalHeader();
			setState(54);
			instructionBlock();
			setState(55);
			match(DOT);
			setState(56);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PascalHeaderContext extends ParserRuleContext {
		public List<PascalBlockContext> pascalBlock() {
			return getRuleContexts(PascalBlockContext.class);
		}
		public PascalBlockContext pascalBlock(int i) {
			return getRuleContext(PascalBlockContext.class,i);
		}
		public PascalHeaderContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pascalHeader; }
	}

	public final PascalHeaderContext pascalHeader() throws RecognitionException {
		PascalHeaderContext _localctx = new PascalHeaderContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_pascalHeader);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(61);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==CONST || _la==VAR) {
				{
				{
				setState(58);
				pascalBlock();
				}
				}
				setState(63);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PascalBlockContext extends ParserRuleContext {
		public ConstBlockContext constBlock() {
			return getRuleContext(ConstBlockContext.class,0);
		}
		public VarBlockContext varBlock() {
			return getRuleContext(VarBlockContext.class,0);
		}
		public PascalBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pascalBlock; }
	}

	public final PascalBlockContext pascalBlock() throws RecognitionException {
		PascalBlockContext _localctx = new PascalBlockContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_pascalBlock);
		try {
			setState(66);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case CONST:
				enterOuterAlt(_localctx, 1);
				{
				setState(64);
				constBlock();
				}
				break;
			case VAR:
				enterOuterAlt(_localctx, 2);
				{
				setState(65);
				varBlock();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class InstructionBlockContext extends ParserRuleContext {
		public TerminalNode BEGIN() { return getToken(pascalscript_parserParser.BEGIN, 0); }
		public TerminalNode END() { return getToken(pascalscript_parserParser.END, 0); }
		public List<InstructionContext> instruction() {
			return getRuleContexts(InstructionContext.class);
		}
		public InstructionContext instruction(int i) {
			return getRuleContext(InstructionContext.class,i);
		}
		public InstructionBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_instructionBlock; }
	}

	public final InstructionBlockContext instructionBlock() throws RecognitionException {
		InstructionBlockContext _localctx = new InstructionBlockContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_instructionBlock);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(68);
			match(BEGIN);
			setState(72);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((((_la - 15)) & ~0x3f) == 0 && ((1L << (_la - 15)) & 36028797018963971L) != 0)) {
				{
				{
				setState(69);
				instruction();
				}
				}
				setState(74);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(75);
			match(END);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class InstructionContext extends ParserRuleContext {
		public AssignmentContext assignment() {
			return getRuleContext(AssignmentContext.class,0);
		}
		public ProcedureCallContext procedureCall() {
			return getRuleContext(ProcedureCallContext.class,0);
		}
		public InstructionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_instruction; }
	}

	public final InstructionContext instruction() throws RecognitionException {
		InstructionContext _localctx = new InstructionContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_instruction);
		try {
			setState(79);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENTIFIER:
				enterOuterAlt(_localctx, 1);
				{
				setState(77);
				assignment();
				}
				break;
			case WRITE:
			case WRITELN:
				enterOuterAlt(_localctx, 2);
				{
				setState(78);
				procedureCall();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AssignmentContext extends ParserRuleContext {
		public VariableReferenceContext variableReference() {
			return getRuleContext(VariableReferenceContext.class,0);
		}
		public TerminalNode DOT_COLON() { return getToken(pascalscript_parserParser.DOT_COLON, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode SEMI_COLON() { return getToken(pascalscript_parserParser.SEMI_COLON, 0); }
		public AssignmentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assignment; }
	}

	public final AssignmentContext assignment() throws RecognitionException {
		AssignmentContext _localctx = new AssignmentContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_assignment);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(81);
			variableReference();
			setState(82);
			match(DOT_COLON);
			setState(83);
			expression(0);
			setState(84);
			match(SEMI_COLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcedureCallContext extends ParserRuleContext {
		public TerminalNode SEMI_COLON() { return getToken(pascalscript_parserParser.SEMI_COLON, 0); }
		public TerminalNode WRITE() { return getToken(pascalscript_parserParser.WRITE, 0); }
		public TerminalNode WRITELN() { return getToken(pascalscript_parserParser.WRITELN, 0); }
		public TerminalNode LEFT_PARENTHESIS() { return getToken(pascalscript_parserParser.LEFT_PARENTHESIS, 0); }
		public ParameterListContext parameterList() {
			return getRuleContext(ParameterListContext.class,0);
		}
		public TerminalNode RIGHT_PARENTHESIS() { return getToken(pascalscript_parserParser.RIGHT_PARENTHESIS, 0); }
		public ProcedureCallContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procedureCall; }
	}

	public final ProcedureCallContext procedureCall() throws RecognitionException {
		ProcedureCallContext _localctx = new ProcedureCallContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_procedureCall);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(86);
			_la = _input.LA(1);
			if ( !(_la==WRITE || _la==WRITELN) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(91);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LEFT_PARENTHESIS) {
				{
				setState(87);
				match(LEFT_PARENTHESIS);
				setState(88);
				parameterList();
				setState(89);
				match(RIGHT_PARENTHESIS);
				}
			}

			setState(93);
			match(SEMI_COLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParameterListContext extends ParserRuleContext {
		public List<ParameterContext> parameter() {
			return getRuleContexts(ParameterContext.class);
		}
		public ParameterContext parameter(int i) {
			return getRuleContext(ParameterContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(pascalscript_parserParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(pascalscript_parserParser.COMMA, i);
		}
		public ParameterListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parameterList; }
	}

	public final ParameterListContext parameterList() throws RecognitionException {
		ParameterListContext _localctx = new ParameterListContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_parameterList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(95);
			parameter();
			setState(100);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(96);
				match(COMMA);
				setState(97);
				parameter();
				}
				}
				setState(102);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParameterContext extends ParserRuleContext {
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public VariableReferenceContext variableReference() {
			return getRuleContext(VariableReferenceContext.class,0);
		}
		public ConstantReferenceContext constantReference() {
			return getRuleContext(ConstantReferenceContext.class,0);
		}
		public ParameterContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parameter; }
	}

	public final ParameterContext parameter() throws RecognitionException {
		ParameterContext _localctx = new ParameterContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_parameter);
		try {
			setState(106);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(103);
				expression(0);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(104);
				variableReference();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(105);
				constantReference();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ConstBlockContext extends ParserRuleContext {
		public TerminalNode CONST() { return getToken(pascalscript_parserParser.CONST, 0); }
		public List<ConstantDeclarationContext> constantDeclaration() {
			return getRuleContexts(ConstantDeclarationContext.class);
		}
		public ConstantDeclarationContext constantDeclaration(int i) {
			return getRuleContext(ConstantDeclarationContext.class,i);
		}
		public ConstBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constBlock; }
	}

	public final ConstBlockContext constBlock() throws RecognitionException {
		ConstBlockContext _localctx = new ConstBlockContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_constBlock);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(108);
			match(CONST);
			setState(110); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(109);
				constantDeclaration();
				}
				}
				setState(112); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==IDENTIFIER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ConstantDeclarationContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(pascalscript_parserParser.IDENTIFIER, 0); }
		public TerminalNode EQUAL() { return getToken(pascalscript_parserParser.EQUAL, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode SEMI_COLON() { return getToken(pascalscript_parserParser.SEMI_COLON, 0); }
		public ConstantDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constantDeclaration; }
	}

	public final ConstantDeclarationContext constantDeclaration() throws RecognitionException {
		ConstantDeclarationContext _localctx = new ConstantDeclarationContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_constantDeclaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(114);
			match(IDENTIFIER);
			setState(115);
			match(EQUAL);
			setState(116);
			expression(0);
			setState(117);
			match(SEMI_COLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeReferenceContext extends ParserRuleContext {
		public ScalarTypeContext scalarType() {
			return getRuleContext(ScalarTypeContext.class,0);
		}
		public TypeReferenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeReference; }
	}

	public final TypeReferenceContext typeReference() throws RecognitionException {
		TypeReferenceContext _localctx = new TypeReferenceContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_typeReference);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			scalarType();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ScalarTypeContext extends ParserRuleContext {
		public TerminalNode INTEGER() { return getToken(pascalscript_parserParser.INTEGER, 0); }
		public TerminalNode CARDINAL() { return getToken(pascalscript_parserParser.CARDINAL, 0); }
		public TerminalNode BOOLEAN() { return getToken(pascalscript_parserParser.BOOLEAN, 0); }
		public TerminalNode CHAR() { return getToken(pascalscript_parserParser.CHAR, 0); }
		public ScalarTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_scalarType; }
	}

	public final ScalarTypeContext scalarType() throws RecognitionException {
		ScalarTypeContext _localctx = new ScalarTypeContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_scalarType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(121);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 31457280L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VarBlockContext extends ParserRuleContext {
		public TerminalNode VAR() { return getToken(pascalscript_parserParser.VAR, 0); }
		public List<VariableDeclarationContext> variableDeclaration() {
			return getRuleContexts(VariableDeclarationContext.class);
		}
		public VariableDeclarationContext variableDeclaration(int i) {
			return getRuleContext(VariableDeclarationContext.class,i);
		}
		public VarBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varBlock; }
	}

	public final VarBlockContext varBlock() throws RecognitionException {
		VarBlockContext _localctx = new VarBlockContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_varBlock);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(123);
			match(VAR);
			setState(125); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(124);
				variableDeclaration();
				}
				}
				setState(127); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==IDENTIFIER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VariableDeclarationContext extends ParserRuleContext {
		public IdentifierListContext identifierList() {
			return getRuleContext(IdentifierListContext.class,0);
		}
		public TerminalNode COLON() { return getToken(pascalscript_parserParser.COLON, 0); }
		public TypeReferenceContext typeReference() {
			return getRuleContext(TypeReferenceContext.class,0);
		}
		public TerminalNode SEMI_COLON() { return getToken(pascalscript_parserParser.SEMI_COLON, 0); }
		public VariableDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variableDeclaration; }
	}

	public final VariableDeclarationContext variableDeclaration() throws RecognitionException {
		VariableDeclarationContext _localctx = new VariableDeclarationContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_variableDeclaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(129);
			identifierList();
			setState(130);
			match(COLON);
			setState(131);
			typeReference();
			setState(132);
			match(SEMI_COLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class IdentifierListContext extends ParserRuleContext {
		public List<TerminalNode> IDENTIFIER() { return getTokens(pascalscript_parserParser.IDENTIFIER); }
		public TerminalNode IDENTIFIER(int i) {
			return getToken(pascalscript_parserParser.IDENTIFIER, i);
		}
		public List<TerminalNode> COMMA() { return getTokens(pascalscript_parserParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(pascalscript_parserParser.COMMA, i);
		}
		public IdentifierListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_identifierList; }
	}

	public final IdentifierListContext identifierList() throws RecognitionException {
		IdentifierListContext _localctx = new IdentifierListContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_identifierList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(134);
			match(IDENTIFIER);
			setState(139);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(135);
				match(COMMA);
				setState(136);
				match(IDENTIFIER);
				}
				}
				setState(141);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExpressionContext extends ParserRuleContext {
		public UnaryOperatorContext unaryOperator() {
			return getRuleContext(UnaryOperatorContext.class,0);
		}
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public TerminalNode UNSIGNED_INTEGER_VALUE() { return getToken(pascalscript_parserParser.UNSIGNED_INTEGER_VALUE, 0); }
		public TerminalNode UNSIGNED_REAL_VALUE() { return getToken(pascalscript_parserParser.UNSIGNED_REAL_VALUE, 0); }
		public TerminalNode CHARACTER_VALUE() { return getToken(pascalscript_parserParser.CHARACTER_VALUE, 0); }
		public TerminalNode BOOLEAN_VALUE() { return getToken(pascalscript_parserParser.BOOLEAN_VALUE, 0); }
		public TerminalNode COMPOSED_STRING_VALUE() { return getToken(pascalscript_parserParser.COMPOSED_STRING_VALUE, 0); }
		public VariableReferenceContext variableReference() {
			return getRuleContext(VariableReferenceContext.class,0);
		}
		public ConstantReferenceContext constantReference() {
			return getRuleContext(ConstantReferenceContext.class,0);
		}
		public TerminalNode LEFT_PARENTHESIS() { return getToken(pascalscript_parserParser.LEFT_PARENTHESIS, 0); }
		public TerminalNode RIGHT_PARENTHESIS() { return getToken(pascalscript_parserParser.RIGHT_PARENTHESIS, 0); }
		public MultiplicativeOperatorContext multiplicativeOperator() {
			return getRuleContext(MultiplicativeOperatorContext.class,0);
		}
		public AdditiveOperatorContext additiveOperator() {
			return getRuleContext(AdditiveOperatorContext.class,0);
		}
		public RelationalOperatorContext relationalOperator() {
			return getRuleContext(RelationalOperatorContext.class,0);
		}
		public ExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expression; }
	}

	public final ExpressionContext expression() throws RecognitionException {
		return expression(0);
	}

	private ExpressionContext expression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExpressionContext _localctx = new ExpressionContext(_ctx, _parentState);
		ExpressionContext _prevctx = _localctx;
		int _startState = 34;
		enterRecursionRule(_localctx, 34, RULE_expression, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(157);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				{
				setState(143);
				unaryOperator();
				setState(144);
				expression(12);
				}
				break;
			case 2:
				{
				setState(146);
				match(UNSIGNED_INTEGER_VALUE);
				}
				break;
			case 3:
				{
				setState(147);
				match(UNSIGNED_REAL_VALUE);
				}
				break;
			case 4:
				{
				setState(148);
				match(CHARACTER_VALUE);
				}
				break;
			case 5:
				{
				setState(149);
				match(BOOLEAN_VALUE);
				}
				break;
			case 6:
				{
				setState(150);
				match(COMPOSED_STRING_VALUE);
				}
				break;
			case 7:
				{
				setState(151);
				variableReference();
				}
				break;
			case 8:
				{
				setState(152);
				constantReference();
				}
				break;
			case 9:
				{
				setState(153);
				match(LEFT_PARENTHESIS);
				setState(154);
				expression(0);
				setState(155);
				match(RIGHT_PARENTHESIS);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(173);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,12,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(171);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,11,_ctx) ) {
					case 1:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(159);
						if (!(precpred(_ctx, 11))) throw new FailedPredicateException(this, "precpred(_ctx, 11)");
						setState(160);
						multiplicativeOperator();
						setState(161);
						expression(12);
						}
						break;
					case 2:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(163);
						if (!(precpred(_ctx, 10))) throw new FailedPredicateException(this, "precpred(_ctx, 10)");
						setState(164);
						additiveOperator();
						setState(165);
						expression(11);
						}
						break;
					case 3:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(167);
						if (!(precpred(_ctx, 9))) throw new FailedPredicateException(this, "precpred(_ctx, 9)");
						setState(168);
						relationalOperator();
						setState(169);
						expression(10);
						}
						break;
					}
					} 
				}
				setState(175);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,12,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class UnaryOperatorContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(pascalscript_parserParser.PLUS, 0); }
		public TerminalNode MINUS() { return getToken(pascalscript_parserParser.MINUS, 0); }
		public TerminalNode NOT() { return getToken(pascalscript_parserParser.NOT, 0); }
		public UnaryOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unaryOperator; }
	}

	public final UnaryOperatorContext unaryOperator() throws RecognitionException {
		UnaryOperatorContext _localctx = new UnaryOperatorContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_unaryOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(176);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 26422638804992L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RelationalOperatorContext extends ParserRuleContext {
		public TerminalNode EQUAL() { return getToken(pascalscript_parserParser.EQUAL, 0); }
		public TerminalNode NOT_EQUAL() { return getToken(pascalscript_parserParser.NOT_EQUAL, 0); }
		public TerminalNode LESS_THAN() { return getToken(pascalscript_parserParser.LESS_THAN, 0); }
		public TerminalNode LESS_OR_EQUAL() { return getToken(pascalscript_parserParser.LESS_OR_EQUAL, 0); }
		public TerminalNode GREATER_THAN() { return getToken(pascalscript_parserParser.GREATER_THAN, 0); }
		public TerminalNode GREATER_OR_EQUAL() { return getToken(pascalscript_parserParser.GREATER_OR_EQUAL, 0); }
		public RelationalOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_relationalOperator; }
	}

	public final RelationalOperatorContext relationalOperator() throws RecognitionException {
		RelationalOperatorContext _localctx = new RelationalOperatorContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_relationalOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(178);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 8866461766385664L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AdditiveOperatorContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(pascalscript_parserParser.PLUS, 0); }
		public TerminalNode MINUS() { return getToken(pascalscript_parserParser.MINUS, 0); }
		public TerminalNode OR() { return getToken(pascalscript_parserParser.OR, 0); }
		public TerminalNode XOR() { return getToken(pascalscript_parserParser.XOR, 0); }
		public AdditiveOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_additiveOperator; }
	}

	public final AdditiveOperatorContext additiveOperator() throws RecognitionException {
		AdditiveOperatorContext _localctx = new AdditiveOperatorContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_additiveOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(180);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 26594437496832L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class MultiplicativeOperatorContext extends ParserRuleContext {
		public TerminalNode STAR() { return getToken(pascalscript_parserParser.STAR, 0); }
		public TerminalNode SLASH() { return getToken(pascalscript_parserParser.SLASH, 0); }
		public TerminalNode DIV() { return getToken(pascalscript_parserParser.DIV, 0); }
		public TerminalNode MOD() { return getToken(pascalscript_parserParser.MOD, 0); }
		public TerminalNode AND() { return getToken(pascalscript_parserParser.AND, 0); }
		public TerminalNode SHL() { return getToken(pascalscript_parserParser.SHL, 0); }
		public TerminalNode SHR() { return getToken(pascalscript_parserParser.SHR, 0); }
		public MultiplicativeOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_multiplicativeOperator; }
	}

	public final MultiplicativeOperatorContext multiplicativeOperator() throws RecognitionException {
		MultiplicativeOperatorContext _localctx = new MultiplicativeOperatorContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_multiplicativeOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(182);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 114074331381760L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VariableReferenceContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(pascalscript_parserParser.IDENTIFIER, 0); }
		public VariableReferenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variableReference; }
	}

	public final VariableReferenceContext variableReference() throws RecognitionException {
		VariableReferenceContext _localctx = new VariableReferenceContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_variableReference);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(184);
			match(IDENTIFIER);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ConstantReferenceContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(pascalscript_parserParser.IDENTIFIER, 0); }
		public ConstantReferenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constantReference; }
	}

	public final ConstantReferenceContext constantReference() throws RecognitionException {
		ConstantReferenceContext _localctx = new ConstantReferenceContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_constantReference);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(186);
			match(IDENTIFIER);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 17:
			return expression_sempred((ExpressionContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expression_sempred(ExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 11);
		case 1:
			return precpred(_ctx, 10);
		case 2:
			return precpred(_ctx, 9);
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u0001^\u00bd\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012"+
		"\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015"+
		"\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0001\u0000\u0001\u0000"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0002\u0005\u0002<\b\u0002\n\u0002\f\u0002"+
		"?\t\u0002\u0001\u0003\u0001\u0003\u0003\u0003C\b\u0003\u0001\u0004\u0001"+
		"\u0004\u0005\u0004G\b\u0004\n\u0004\f\u0004J\t\u0004\u0001\u0004\u0001"+
		"\u0004\u0001\u0005\u0001\u0005\u0003\u0005P\b\u0005\u0001\u0006\u0001"+
		"\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001"+
		"\u0007\u0001\u0007\u0001\u0007\u0003\u0007\\\b\u0007\u0001\u0007\u0001"+
		"\u0007\u0001\b\u0001\b\u0001\b\u0005\bc\b\b\n\b\f\bf\t\b\u0001\t\u0001"+
		"\t\u0001\t\u0003\tk\b\t\u0001\n\u0001\n\u0004\no\b\n\u000b\n\f\np\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\f\u0001\f"+
		"\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0004\u000e~\b\u000e\u000b\u000e"+
		"\f\u000e\u007f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u0010\u0001\u0010\u0001\u0010\u0005\u0010\u008a\b\u0010\n\u0010"+
		"\f\u0010\u008d\t\u0010\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011"+
		"\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011"+
		"\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0003\u0011"+
		"\u009e\b\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011"+
		"\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011"+
		"\u0001\u0011\u0005\u0011\u00ac\b\u0011\n\u0011\f\u0011\u00af\t\u0011\u0001"+
		"\u0012\u0001\u0012\u0001\u0013\u0001\u0013\u0001\u0014\u0001\u0014\u0001"+
		"\u0015\u0001\u0015\u0001\u0016\u0001\u0016\u0001\u0017\u0001\u0017\u0001"+
		"\u0017\u0000\u0001\"\u0018\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012"+
		"\u0014\u0016\u0018\u001a\u001c\u001e \"$&(*,.\u0000\u0006\u0001\u0000"+
		"\u000f\u0010\u0001\u0000\u0015\u0018\u0002\u0000##+,\u0001\u0000/4\u0002"+
		"\u0000$%+,\u0002\u0000&*-.\u00ba\u00000\u0001\u0000\u0000\u0000\u0002"+
		"2\u0001\u0000\u0000\u0000\u0004=\u0001\u0000\u0000\u0000\u0006B\u0001"+
		"\u0000\u0000\u0000\bD\u0001\u0000\u0000\u0000\nO\u0001\u0000\u0000\u0000"+
		"\fQ\u0001\u0000\u0000\u0000\u000eV\u0001\u0000\u0000\u0000\u0010_\u0001"+
		"\u0000\u0000\u0000\u0012j\u0001\u0000\u0000\u0000\u0014l\u0001\u0000\u0000"+
		"\u0000\u0016r\u0001\u0000\u0000\u0000\u0018w\u0001\u0000\u0000\u0000\u001a"+
		"y\u0001\u0000\u0000\u0000\u001c{\u0001\u0000\u0000\u0000\u001e\u0081\u0001"+
		"\u0000\u0000\u0000 \u0086\u0001\u0000\u0000\u0000\"\u009d\u0001\u0000"+
		"\u0000\u0000$\u00b0\u0001\u0000\u0000\u0000&\u00b2\u0001\u0000\u0000\u0000"+
		"(\u00b4\u0001\u0000\u0000\u0000*\u00b6\u0001\u0000\u0000\u0000,\u00b8"+
		"\u0001\u0000\u0000\u0000.\u00ba\u0001\u0000\u0000\u000001\u0003\u0002"+
		"\u0001\u00001\u0001\u0001\u0000\u0000\u000023\u0005\u0002\u0000\u0000"+
		"34\u0005F\u0000\u000045\u0005@\u0000\u000056\u0003\u0004\u0002\u00006"+
		"7\u0003\b\u0004\u000078\u0005;\u0000\u000089\u0005\u0000\u0000\u00019"+
		"\u0003\u0001\u0000\u0000\u0000:<\u0003\u0006\u0003\u0000;:\u0001\u0000"+
		"\u0000\u0000<?\u0001\u0000\u0000\u0000=;\u0001\u0000\u0000\u0000=>\u0001"+
		"\u0000\u0000\u0000>\u0005\u0001\u0000\u0000\u0000?=\u0001\u0000\u0000"+
		"\u0000@C\u0003\u0014\n\u0000AC\u0003\u001c\u000e\u0000B@\u0001\u0000\u0000"+
		"\u0000BA\u0001\u0000\u0000\u0000C\u0007\u0001\u0000\u0000\u0000DH\u0005"+
		"\u0003\u0000\u0000EG\u0003\n\u0005\u0000FE\u0001\u0000\u0000\u0000GJ\u0001"+
		"\u0000\u0000\u0000HF\u0001\u0000\u0000\u0000HI\u0001\u0000\u0000\u0000"+
		"IK\u0001\u0000\u0000\u0000JH\u0001\u0000\u0000\u0000KL\u0005\u0004\u0000"+
		"\u0000L\t\u0001\u0000\u0000\u0000MP\u0003\f\u0006\u0000NP\u0003\u000e"+
		"\u0007\u0000OM\u0001\u0000\u0000\u0000ON\u0001\u0000\u0000\u0000P\u000b"+
		"\u0001\u0000\u0000\u0000QR\u0003,\u0016\u0000RS\u00055\u0000\u0000ST\u0003"+
		"\"\u0011\u0000TU\u0005@\u0000\u0000U\r\u0001\u0000\u0000\u0000V[\u0007"+
		"\u0000\u0000\u0000WX\u0005=\u0000\u0000XY\u0003\u0010\b\u0000YZ\u0005"+
		"?\u0000\u0000Z\\\u0001\u0000\u0000\u0000[W\u0001\u0000\u0000\u0000[\\"+
		"\u0001\u0000\u0000\u0000\\]\u0001\u0000\u0000\u0000]^\u0005@\u0000\u0000"+
		"^\u000f\u0001\u0000\u0000\u0000_d\u0003\u0012\t\u0000`a\u00059\u0000\u0000"+
		"ac\u0003\u0012\t\u0000b`\u0001\u0000\u0000\u0000cf\u0001\u0000\u0000\u0000"+
		"db\u0001\u0000\u0000\u0000de\u0001\u0000\u0000\u0000e\u0011\u0001\u0000"+
		"\u0000\u0000fd\u0001\u0000\u0000\u0000gk\u0003\"\u0011\u0000hk\u0003,"+
		"\u0016\u0000ik\u0003.\u0017\u0000jg\u0001\u0000\u0000\u0000jh\u0001\u0000"+
		"\u0000\u0000ji\u0001\u0000\u0000\u0000k\u0013\u0001\u0000\u0000\u0000"+
		"ln\u0005\u001e\u0000\u0000mo\u0003\u0016\u000b\u0000nm\u0001\u0000\u0000"+
		"\u0000op\u0001\u0000\u0000\u0000pn\u0001\u0000\u0000\u0000pq\u0001\u0000"+
		"\u0000\u0000q\u0015\u0001\u0000\u0000\u0000rs\u0005F\u0000\u0000st\u0005"+
		"/\u0000\u0000tu\u0003\"\u0011\u0000uv\u0005@\u0000\u0000v\u0017\u0001"+
		"\u0000\u0000\u0000wx\u0003\u001a\r\u0000x\u0019\u0001\u0000\u0000\u0000"+
		"yz\u0007\u0001\u0000\u0000z\u001b\u0001\u0000\u0000\u0000{}\u0005 \u0000"+
		"\u0000|~\u0003\u001e\u000f\u0000}|\u0001\u0000\u0000\u0000~\u007f\u0001"+
		"\u0000\u0000\u0000\u007f}\u0001\u0000\u0000\u0000\u007f\u0080\u0001\u0000"+
		"\u0000\u0000\u0080\u001d\u0001\u0000\u0000\u0000\u0081\u0082\u0003 \u0010"+
		"\u0000\u0082\u0083\u00058\u0000\u0000\u0083\u0084\u0003\u0018\f\u0000"+
		"\u0084\u0085\u0005@\u0000\u0000\u0085\u001f\u0001\u0000\u0000\u0000\u0086"+
		"\u008b\u0005F\u0000\u0000\u0087\u0088\u00059\u0000\u0000\u0088\u008a\u0005"+
		"F\u0000\u0000\u0089\u0087\u0001\u0000\u0000\u0000\u008a\u008d\u0001\u0000"+
		"\u0000\u0000\u008b\u0089\u0001\u0000\u0000\u0000\u008b\u008c\u0001\u0000"+
		"\u0000\u0000\u008c!\u0001\u0000\u0000\u0000\u008d\u008b\u0001\u0000\u0000"+
		"\u0000\u008e\u008f\u0006\u0011\uffff\uffff\u0000\u008f\u0090\u0003$\u0012"+
		"\u0000\u0090\u0091\u0003\"\u0011\f\u0091\u009e\u0001\u0000\u0000\u0000"+
		"\u0092\u009e\u0005I\u0000\u0000\u0093\u009e\u0005J\u0000\u0000\u0094\u009e"+
		"\u0005T\u0000\u0000\u0095\u009e\u0005Z\u0000\u0000\u0096\u009e\u0005U"+
		"\u0000\u0000\u0097\u009e\u0003,\u0016\u0000\u0098\u009e\u0003.\u0017\u0000"+
		"\u0099\u009a\u0005=\u0000\u0000\u009a\u009b\u0003\"\u0011\u0000\u009b"+
		"\u009c\u0005?\u0000\u0000\u009c\u009e\u0001\u0000\u0000\u0000\u009d\u008e"+
		"\u0001\u0000\u0000\u0000\u009d\u0092\u0001\u0000\u0000\u0000\u009d\u0093"+
		"\u0001\u0000\u0000\u0000\u009d\u0094\u0001\u0000\u0000\u0000\u009d\u0095"+
		"\u0001\u0000\u0000\u0000\u009d\u0096\u0001\u0000\u0000\u0000\u009d\u0097"+
		"\u0001\u0000\u0000\u0000\u009d\u0098\u0001\u0000\u0000\u0000\u009d\u0099"+
		"\u0001\u0000\u0000\u0000\u009e\u00ad\u0001\u0000\u0000\u0000\u009f\u00a0"+
		"\n\u000b\u0000\u0000\u00a0\u00a1\u0003*\u0015\u0000\u00a1\u00a2\u0003"+
		"\"\u0011\f\u00a2\u00ac\u0001\u0000\u0000\u0000\u00a3\u00a4\n\n\u0000\u0000"+
		"\u00a4\u00a5\u0003(\u0014\u0000\u00a5\u00a6\u0003\"\u0011\u000b\u00a6"+
		"\u00ac\u0001\u0000\u0000\u0000\u00a7\u00a8\n\t\u0000\u0000\u00a8\u00a9"+
		"\u0003&\u0013\u0000\u00a9\u00aa\u0003\"\u0011\n\u00aa\u00ac\u0001\u0000"+
		"\u0000\u0000\u00ab\u009f\u0001\u0000\u0000\u0000\u00ab\u00a3\u0001\u0000"+
		"\u0000\u0000\u00ab\u00a7\u0001\u0000\u0000\u0000\u00ac\u00af\u0001\u0000"+
		"\u0000\u0000\u00ad\u00ab\u0001\u0000\u0000\u0000\u00ad\u00ae\u0001\u0000"+
		"\u0000\u0000\u00ae#\u0001\u0000\u0000\u0000\u00af\u00ad\u0001\u0000\u0000"+
		"\u0000\u00b0\u00b1\u0007\u0002\u0000\u0000\u00b1%\u0001\u0000\u0000\u0000"+
		"\u00b2\u00b3\u0007\u0003\u0000\u0000\u00b3\'\u0001\u0000\u0000\u0000\u00b4"+
		"\u00b5\u0007\u0004\u0000\u0000\u00b5)\u0001\u0000\u0000\u0000\u00b6\u00b7"+
		"\u0007\u0005\u0000\u0000\u00b7+\u0001\u0000\u0000\u0000\u00b8\u00b9\u0005"+
		"F\u0000\u0000\u00b9-\u0001\u0000\u0000\u0000\u00ba\u00bb\u0005F\u0000"+
		"\u0000\u00bb/\u0001\u0000\u0000\u0000\r=BHO[djp\u007f\u008b\u009d\u00ab"+
		"\u00ad";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}