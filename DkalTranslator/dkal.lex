// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

using TUVienna.CS_CUP.Runtime;

%%
%{
  private int comment_count = 0;
  
  public int getCurrentLineNumer(){
  	return yyline;
  }
%} 
%cup
%line
%char
%state COMMENT
%class Lexicon

ALPHA=[A-Za-z]
ALPHAUPPER=[A-Z]
ALPHALOWER=[a-z]
DIGIT=[0-9]
COMMENT_TEXT=([^/*\n]|[^*\n]"/"[^*\n]|[^/\n]"*"[^/\n]|"*"[^/\n]|"/"[^*\n])*

%%
<YYINITIAL> ";" { return new Symbol(Sym.SEMI); }
<YYINITIAL> "+" { return new Symbol(Sym.PLUS); }
<YYINITIAL> "->" { return new Symbol(Sym.IMPLIES); }
<YYINITIAL> "true" { return new Symbol(Sym.TRUE); }
<YYINITIAL> "knows" { return new Symbol(Sym.KNOWS); }
<YYINITIAL> "said" { return new Symbol(Sym.SAID); }
<YYINITIAL> "put" { return new Symbol(Sym.PUT); }
<YYINITIAL> "to" { return new Symbol(Sym.TO); }
<YYINITIAL> "from" { return new Symbol(Sym.FROM); }
<YYINITIAL> "]" { return new Symbol(Sym.RBRACKET); }
<YYINITIAL> "[" { return new Symbol(Sym.LBRACKET); }
<YYINITIAL> "<-" { return new Symbol(Sym.PROVISO); }
<YYINITIAL> "<=" { return new Symbol(Sym.CONDITIONAL); }
<YYINITIAL> "(" { return new Symbol(Sym.LPAREN); }
<YYINITIAL> ")" { return new Symbol(Sym.RPAREN); }
<YYINITIAL> ":" { return new Symbol(Sym.COLON); }
<YYINITIAL> "?" { return new Symbol(Sym.QUESTION); }
<YYINITIAL> "," { return new Symbol(Sym.COMMA); }
<YYINITIAL> "=" { return new Symbol(Sym.EQUAL); }
<YYINITIAL> "Principals" { return new Symbol(Sym.PRINCIPAL); }
<YYINITIAL> "Functions" { return new Symbol(Sym.FUNCTION); }
<YYINITIAL> "Fix" { return new Symbol(Sym.FIX); }
<YYINITIAL> [ \t\r\n\f] { /* ignore white space. */break; }
<YYINITIAL> {ALPHAUPPER}({ALPHA}|{DIGIT}|_)* { return (new Symbol(Sym.VARIABLE,yytext()));}
<YYINITIAL> {ALPHALOWER}({ALPHA}|{DIGIT}|_)* { return (new Symbol(Sym.TOKEN,yytext()));}
<YYINITIAL> {DIGIT}({DIGIT}|_)* { return (new Symbol(Sym.NUMBER,yytext()));}
<YYINITIAL> "/*" { yybegin(COMMENT); comment_count = comment_count + 1;break; }

<YYINITIAL,COMMENT> \n { }

<COMMENT> "/*" { comment_count = comment_count + 1;break; }
<COMMENT> "*/" { 
	comment_count = comment_count - 1; 
	if (comment_count > 0) {
		throw (new System.Exception("Error: bad commented line"));
	}
	if (comment_count == 0) {
    		yybegin(YYINITIAL);
   
	}
	break;
}
<COMMENT> {COMMENT_TEXT} { }


<YYINITIAL,COMMENT> . { System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
