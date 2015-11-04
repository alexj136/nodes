package parser;

import syntax.Name;
import scala.Tuple2;
import scala.collection.immutable.Map;
import scala.collection.immutable.Map$;
import java_cup.runtime.Symbol;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java.io.Reader;

%%

%public
%class Lexer
%cup
%line
%column

%eofval{
    return symbol(sym.EOF);
%eofval}

%{
    // For symbol generation
    private ComplexSymbolFactory csf;
    public ComplexSymbolFactory csf() { return this.csf; }

    // Used in converting identifier strings to integers
    private Map<String, Name> nameMap;
    private Name nextAvailableName;

    private Lexer(Reader input, ComplexSymbolFactory csf) {
        this(input);
        this.csf = csf;
        this.nameMap = Map$.MODULE$.<String, Name>empty();
        this.nextAvailableName = new Name(0);
    }

    /**
     * Build a Lexer.
     * @param input the InputStreamReader to take input from
     */
    public static Lexer create(Reader input) {
        return new Lexer(input, new ComplexSymbolFactory());
    }

    /**
     * Access the nameMap and nextAvailableName for subsequent usage.
     * @return the nameMap and nextAvailableName as a pair
     */
    public Tuple2<Map<String, Name>, Name> getNameInfo() {
        return new Tuple2<Map<String, Name>, Name>
                (this.nameMap, this.nextAvailableName);
    }

    /**
     * Construct a new Symbol containing the symbol type, not caring about the
     * text.
     * @param type the token type
     * @return a new Symbol with type information
     */
    public Symbol symbol(int type) {
        Tuple2<Location, Location> loc = this.getLexerLocation();
        return csf.newSymbol(sym.terminalNames[type], type, loc._1, loc._2);
    }

    /**
     * Construct a new Symbol containing the symbol type and scanned text.
     * @param type the token type
     * @param text the token text
     * @return a new Symbol with type and text information. The text information
     * is an int that is a key in the nameMap, and can be used to look up the
     * actual String value.
     */
    public Symbol symbol(int type, String text) {
        Tuple2<Location, Location> loc = this.getLexerLocation();
        return csf.newSymbol(sym.terminalNames[type], type, loc._1, loc._2,
                type == sym.INT ? Integer.parseInt(text) : this.lease(text));
    }

    /**
     * Obtain the start and end locations of the current token.
     * @return the start and end locations of the current token
     */
    public Tuple2<Location, Location> getLexerLocation() {
        Location left = new Location(yyline + 1, yycolumn + 1, yychar);
        Location right = new Location(yyline + 1, yycolumn + yylength(),
                yychar + yylength());
        return new Tuple2<Location, Location>(left, right);
    }

    /**
     * Convert an identifier string into an integer. If we haven't seen the
     * string before, associate the string with an unused integer, returning
     * that integer. If we have seen the string before, return the integer
     * already associated with it.
     * @param strName the String to use to obtain an intname
     * @return an int that corresponds to the given String name
     */
    public Name lease(String strName) {
        if(this.nameMap.contains(strName)) {
            return this.nameMap.apply(strName);
        }
        else {
            Name toLease = nextAvailableName;
            nextAvailableName = toLease.next();
            this.nameMap = this.nameMap.updated(strName, toLease);
            return toLease;
        }
    }
%}

Ident   = [a-z_]+
Chan    = "$"[a-z_]+
Int     = 0|[1-9][0-9]*
NewLine = \r|\n|\r\n;
Space   = {NewLine} | [ \t\f]
Comment = "//"[^\r\n]* {NewLine}?

%%

<YYINITIAL>{

    "!"       { return symbol ( sym.BANG             ); }
    "*"       { return symbol ( sym.STAR             ); }
    "."       { return symbol ( sym.DOT              ); }
    ":"       { return symbol ( sym.COLON            ); }
    "let"     { return symbol ( sym.LET              ); }
    "new"     { return symbol ( sym.NEW              ); }
    "if"      { return symbol ( sym.IF               ); }
    "then"    { return symbol ( sym.THEN             ); }
    "else"    { return symbol ( sym.ELSE             ); }
    "endif"   { return symbol ( sym.ENDIF            ); }
    "send"    { return symbol ( sym.SEND             ); }
    "receive" { return symbol ( sym.RECEIVE          ); }
    "server"  { return symbol ( sym.SERVER           ); }
    "|"       { return symbol ( sym.BAR              ); }
    "end"     { return symbol ( sym.END              ); }
    "("       { return symbol ( sym.LPAREN           ); }
    ")"       { return symbol ( sym.RPAREN           ); }
    "{"       { return symbol ( sym.LCURLY           ); }
    "}"       { return symbol ( sym.RCURLY           ); }
    ","       { return symbol ( sym.COMMA            ); }
    "<-"      { return symbol ( sym.LARROW           ); }
    "->"      { return symbol ( sym.RARROW           ); }
    "true"    { return symbol ( sym.TRUE             ); }
    "false"   { return symbol ( sym.FALSE            ); }
    "+"       { return symbol ( sym.PLUS             ); }
    "-"       { return symbol ( sym.DASH             ); }
    "/"       { return symbol ( sym.FSLASH           ); }
    "%"       { return symbol ( sym.PERC             ); }
    "="       { return symbol ( sym.EQUAL            ); }
    "=="      { return symbol ( sym.EQEQ             ); }
    "!="      { return symbol ( sym.NEQ              ); }
    "<"       { return symbol ( sym.LESS             ); }
    "<="      { return symbol ( sym.LESSEQ           ); }
    ">"       { return symbol ( sym.GRTR             ); }
    ">="      { return symbol ( sym.GRTREQ           ); }
    "&&"      { return symbol ( sym.AND              ); }
    "||"      { return symbol ( sym.OR               ); }
    {Ident}   { return symbol ( sym.IDENT , yytext() ); }
    {Chan}    { return symbol ( sym.CHAN  , yytext() ); }
    {Int}     { return symbol ( sym.INT   , yytext() ); }
    {Space}   { /* ignore */                            }
    {Comment} { /* ignore */                            }
    [^]|\n    { return symbol ( sym.ERROR            ); }
}
