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


public class Lexicon : TUVienna.CS_CUP.Runtime.Scanner {
	private const int YY_BUFFER_SIZE = 512;
	private const int YY_F = -1;
	private const int YY_NO_STATE = -1;
	private const int YY_NOT_ACCEPT = 0;
	private const int YY_START = 1;
	private const int YY_END = 2;
	private const int YY_NO_ANCHOR = 4;
	private const int YY_BOL = 128;
	private const int YY_EOF = 129;

  private int comment_count = 0;
  public int getCurrentLineNumer(){
  	return yyline;
  }
	private System.IO.TextReader yy_reader;
	private int yy_buffer_index;
	private int yy_buffer_read;
	private int yy_buffer_start;
	private int yy_buffer_end;
	private char[] yy_buffer;
	private int yychar;
	private int yyline;
	private bool yy_at_bol;
	private int yy_lexical_state;

	public Lexicon (System.IO.TextReader yy_reader1) : this() {
		if (null == yy_reader1) {
			throw (new System.Exception("Error: Bad input stream initializer."));
		}
		yy_reader = yy_reader1;
	}

	private Lexicon () {
		yy_buffer = new char[YY_BUFFER_SIZE];
		yy_buffer_read = 0;
		yy_buffer_index = 0;
		yy_buffer_start = 0;
		yy_buffer_end = 0;
		yychar = 0;
		yyline = 0;
		yy_at_bol = true;
		yy_lexical_state = YYINITIAL;
	}

	private bool yy_eof_done = false;
	private const int COMMENT = 1;
	private const int YYINITIAL = 0;
	private static readonly int[] yy_state_dtrans =new int[] {
		0,
		30
	};
	private void yybegin (int state) {
		yy_lexical_state = state;
	}
	private int yy_advance ()
	{
		int next_read;
		int i;
		int j;

		if (yy_buffer_index < yy_buffer_read) {
			return yy_buffer[yy_buffer_index++];
		}

		if (0 != yy_buffer_start) {
			i = yy_buffer_start;
			j = 0;
			while (i < yy_buffer_read) {
				yy_buffer[j] = yy_buffer[i];
				++i;
				++j;
			}
			yy_buffer_end = yy_buffer_end - yy_buffer_start;
			yy_buffer_start = 0;
			yy_buffer_read = j;
			yy_buffer_index = j;
			next_read = yy_reader.Read(yy_buffer,
					yy_buffer_read,
					yy_buffer.Length - yy_buffer_read);
			if ( next_read<=0) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}

		while (yy_buffer_index >= yy_buffer_read) {
			if (yy_buffer_index >= yy_buffer.Length) {
				yy_buffer = yy_double(yy_buffer);
			}
			next_read = yy_reader.Read(yy_buffer,
					yy_buffer_read,
					yy_buffer.Length - yy_buffer_read);
			if ( next_read<=0) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}
		return yy_buffer[yy_buffer_index++];
	}
	private void yy_move_end () {
		if (yy_buffer_end > yy_buffer_start &&
		    '\n' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
		if (yy_buffer_end > yy_buffer_start &&
		    '\r' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
	}
	private bool yy_last_was_cr=false;
	private void yy_mark_start () {
		int i;
		for (i = yy_buffer_start; i < yy_buffer_index; ++i) {
			if ('\n' == yy_buffer[i] && !yy_last_was_cr) {
				++yyline;
			}
			if ('\r' == yy_buffer[i]) {
				++yyline;
				yy_last_was_cr=true;
			} else yy_last_was_cr=false;
		}
		yychar = yychar
			+ yy_buffer_index - yy_buffer_start;
		yy_buffer_start = yy_buffer_index;
	}
	private void yy_mark_end () {
		yy_buffer_end = yy_buffer_index;
	}
	private void yy_to_mark () {
		yy_buffer_index = yy_buffer_end;
		yy_at_bol = (yy_buffer_end > yy_buffer_start) &&
		            ('\r' == yy_buffer[yy_buffer_end-1] ||
		             '\n' == yy_buffer[yy_buffer_end-1] ||
		             2028/*LS*/ == yy_buffer[yy_buffer_end-1] ||
		             2029/*PS*/ == yy_buffer[yy_buffer_end-1]);
	}
	private string yytext () {
		return (new string(yy_buffer,
			yy_buffer_start,
			yy_buffer_end - yy_buffer_start));
	}
	private int yylength () {
		return yy_buffer_end - yy_buffer_start;
	}
	private char[] yy_double (char[] buf) {
		int i;
		char[] newbuf;
		newbuf = new char[2*buf.Length];
		for (i = 0; i < buf.Length; ++i) {
			newbuf[i] = buf[i];
		}
		return newbuf;
	}
	private const int YY_E_INTERNAL = 0;
	private const int YY_E_MATCH = 1;
	private string[] yy_error_string = {
		"Error: Internal error.\n",
		"Error: Unmatched input.\n"
	};
	private void yy_error (int code,bool fatal) {
		 System.Console.Write(yy_error_string[code]);
		 System.Console.Out.Flush();
		if (fatal) {
			throw new System.Exception("Fatal Error.\n");
		}
	}
	private static int[][] unpackFromString(int size1, int size2, string st) {
		int colonIndex = -1;
		string lengthString;
		int sequenceLength = 0;
		int sequenceInteger = 0;

		int commaIndex;
		string workString;

		int[][] res = new int[size1][];
		for(int i=0;i<size1;i++) res[i]=new int[size2];
		for (int i= 0; i < size1; i++) {
			for (int j= 0; j < size2; j++) {
				if (sequenceLength != 0) {
					res[i][j] = sequenceInteger;
					sequenceLength--;
					continue;
				}
				commaIndex = st.IndexOf(',');
				workString = (commaIndex==-1) ? st :
					st.Substring(0, commaIndex);
				st = st.Substring(commaIndex+1);
				colonIndex = workString.IndexOf(':');
				if (colonIndex == -1) {
					res[i][j]=System.Int32.Parse(workString);
					continue;
				}
				lengthString =
					workString.Substring(colonIndex+1);
				sequenceLength=System.Int32.Parse(lengthString);
				workString=workString.Substring(0,colonIndex);
				sequenceInteger=System.Int32.Parse(workString);
				res[i][j] = sequenceInteger;
				sequenceLength--;
			}
		}
		return res;
	}
	private int[] yy_acpt = {
		/* 0 */ YY_NOT_ACCEPT,
		/* 1 */ YY_NO_ANCHOR,
		/* 2 */ YY_NO_ANCHOR,
		/* 3 */ YY_NO_ANCHOR,
		/* 4 */ YY_NO_ANCHOR,
		/* 5 */ YY_NO_ANCHOR,
		/* 6 */ YY_NO_ANCHOR,
		/* 7 */ YY_NO_ANCHOR,
		/* 8 */ YY_NO_ANCHOR,
		/* 9 */ YY_NO_ANCHOR,
		/* 10 */ YY_NO_ANCHOR,
		/* 11 */ YY_NO_ANCHOR,
		/* 12 */ YY_NO_ANCHOR,
		/* 13 */ YY_NO_ANCHOR,
		/* 14 */ YY_NO_ANCHOR,
		/* 15 */ YY_NO_ANCHOR,
		/* 16 */ YY_NO_ANCHOR,
		/* 17 */ YY_NO_ANCHOR,
		/* 18 */ YY_NO_ANCHOR,
		/* 19 */ YY_NO_ANCHOR,
		/* 20 */ YY_NO_ANCHOR,
		/* 21 */ YY_NO_ANCHOR,
		/* 22 */ YY_NO_ANCHOR,
		/* 23 */ YY_NO_ANCHOR,
		/* 24 */ YY_NO_ANCHOR,
		/* 25 */ YY_NO_ANCHOR,
		/* 26 */ YY_NO_ANCHOR,
		/* 27 */ YY_NO_ANCHOR,
		/* 28 */ YY_NO_ANCHOR,
		/* 29 */ YY_NO_ANCHOR,
		/* 30 */ YY_NO_ANCHOR,
		/* 31 */ YY_NO_ANCHOR,
		/* 32 */ YY_NO_ANCHOR,
		/* 33 */ YY_NO_ANCHOR,
		/* 34 */ YY_NOT_ACCEPT,
		/* 35 */ YY_NO_ANCHOR,
		/* 36 */ YY_NO_ANCHOR,
		/* 37 */ YY_NO_ANCHOR,
		/* 38 */ YY_NO_ANCHOR,
		/* 39 */ YY_NOT_ACCEPT,
		/* 40 */ YY_NO_ANCHOR,
		/* 41 */ YY_NO_ANCHOR,
		/* 42 */ YY_NO_ANCHOR,
		/* 43 */ YY_NO_ANCHOR,
		/* 44 */ YY_NO_ANCHOR,
		/* 45 */ YY_NO_ANCHOR,
		/* 46 */ YY_NO_ANCHOR,
		/* 47 */ YY_NO_ANCHOR,
		/* 48 */ YY_NO_ANCHOR,
		/* 49 */ YY_NO_ANCHOR,
		/* 50 */ YY_NO_ANCHOR,
		/* 51 */ YY_NO_ANCHOR,
		/* 52 */ YY_NO_ANCHOR,
		/* 53 */ YY_NO_ANCHOR,
		/* 54 */ YY_NO_ANCHOR,
		/* 55 */ YY_NO_ANCHOR,
		/* 56 */ YY_NO_ANCHOR,
		/* 57 */ YY_NO_ANCHOR,
		/* 58 */ YY_NO_ANCHOR,
		/* 59 */ YY_NO_ANCHOR,
		/* 60 */ YY_NO_ANCHOR,
		/* 61 */ YY_NO_ANCHOR,
		/* 62 */ YY_NO_ANCHOR,
		/* 63 */ YY_NO_ANCHOR,
		/* 64 */ YY_NO_ANCHOR,
		/* 65 */ YY_NO_ANCHOR,
		/* 66 */ YY_NO_ANCHOR,
		/* 67 */ YY_NO_ANCHOR,
		/* 68 */ YY_NO_ANCHOR,
		/* 69 */ YY_NO_ANCHOR,
		/* 70 */ YY_NO_ANCHOR,
		/* 71 */ YY_NO_ANCHOR,
		/* 72 */ YY_NO_ANCHOR,
		/* 73 */ YY_NO_ANCHOR,
		/* 74 */ YY_NO_ANCHOR,
		/* 75 */ YY_NO_ANCHOR,
		/* 76 */ YY_NO_ANCHOR,
		/* 77 */ YY_NO_ANCHOR,
		/* 78 */ YY_NO_ANCHOR,
		/* 79 */ YY_NO_ANCHOR,
		/* 80 */ YY_NO_ANCHOR,
		/* 81 */ YY_NO_ANCHOR
	};
	private int[] yy_cmap = unpackFromString(1,130,
"43:9,35,42,43,35:2,43:18,35,43:7,25,26,41,2,29,3,43,40,38:10,27,1,23,24,4,2" +
"8,43,36:5,33,36:5,32,36:3,30,36:10,22,43,21,43,39,43,14,37,31,16,8,20,37:2," +
"15,37,9,19,17,10,11,18,37,6,13,5,7,37,12,34,37:2,43:5,0:2")[0];

	private int[] yy_rmap = unpackFromString(1,82,
"0,1:3,2,3,1:8,4,1,5,1,6,1:3,7,6:5,7:2,8,1:3,9,1,6,10,11,12,13,14,15,16,17,1" +
"8,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,4" +
"3,44,45,46,47,48,49,50,51,52,7:2")[0];

	private int[][] yy_nxt = unpackFromString(53,44,
"1,2,3,4,35,5,36:3,68,36:3,62,36,75,36:4,64,6,7,40,8,9,10,11,12,13,14,36,77," +
"53,36,15,81,36,16,35,44,35,15,35,-1:48,17,-1:44,36,54,36:4,18,36:9,-1:9,36:" +
"5,-1,36:4,-1:9,81,79,81:14,-1:9,81:5,-1,81:2,80:2,-1:42,16:2,-1:9,36:16,-1:" +
"9,36:5,-1,36:4,-1:9,81:16,-1:9,81:5,-1,81:2,80:2,-1:4,1,38:39,48,50,31,38,-" +
"1,38:39,43,-1:2,38,-1:5,81:16,-1:9,81:4,22,-1,81:2,80:2,-1:5,38:39,34,39,-1" +
",38,-1,38:39,-1,47,-1,38,-1:3,19,-1:20,20,-1:24,36:3,23,36:12,-1:9,36:5,-1," +
"36:4,-1:9,81:8,28,81:7,-1:9,81:5,-1,81:2,80:2,-1:5,38:39,43,39,-1,38,-1:41," +
"21,-1:7,36:11,24,36:4,-1:9,36:5,-1,36:4,-1:9,81:8,29,81:7,-1:9,81:5,-1,81:2" +
",80:2,-1:5,38:39,34,47,-1,38,-1,38:39,43,32,-1,38,-1:5,36:12,25,36:3,-1:9,3" +
"6:5,-1,36:4,-1:5,38:39,33,47,-1,38,-1:5,36:8,26,36:7,-1:9,36:5,-1,36:4,-1:9" +
",36:11,27,36:4,-1:9,36:5,-1,36:4,-1:9,81:10,37,81:5,-1:9,81:5,-1,81:2,80:2," +
"-1:9,36:2,41,36:13,-1:9,36:5,-1,36:4,-1:9,81:14,42,81,-1:9,81:5,-1,81:2,80:" +
"2,-1:9,36:10,45,36:5,-1:9,36:5,-1,36:4,-1:9,81:14,46,81,-1:9,81:5,-1,81:2,8" +
"0:2,-1:9,36:6,49,36:9,-1:9,36:5,-1,36:4,-1:9,36:7,51,36:8,-1:9,36:5,-1,36:4" +
",-1:9,36:3,52,36:12,-1:9,36:5,-1,36:4,-1:9,81:9,55,81:6,-1:9,81:5,-1,81:2,8" +
"0:2,-1:9,36:9,56,36:6,-1:9,36:5,-1,36:4,-1:9,81:9,57,81:6,-1:9,81:5,-1,81:2" +
",80:2,-1:9,36,58,36:14,-1:9,36:5,-1,36:4,-1:9,36:6,59,36:9,-1:9,36:5,-1,36:" +
"4,-1:9,36:10,60,36:5,-1:9,36:5,-1,36:4,-1:9,81,61,81:14,-1:9,81:5,-1,81:2,8" +
"0:2,-1:9,36:5,65,36:10,-1:9,36:5,-1,36:4,-1:9,81:13,63,81:2,-1:9,81:5,-1,81" +
":2,80:2,-1:9,36:14,66,36,-1:9,36:5,-1,36:4,-1:9,81:3,67,81:12,-1:9,81:5,-1," +
"81:2,80:2,-1:9,36:13,70,36:2,-1:9,36:5,-1,36:4,-1:9,81:10,69,81:5,-1:9,81:5" +
",-1,81:2,80:2,-1:9,71,81:15,-1:9,81:5,-1,81:2,80:2,-1:9,36:12,72,36:3,-1:9," +
"36:5,-1,36:4,-1:9,81:16,-1:9,81,73,81:3,-1,81:2,80:2,-1:9,81:10,74,81:5,-1:" +
"9,81:5,-1,81:2,80:2,-1:9,81:5,76,81:10,-1:9,81:5,-1,81:2,80:2,-1:9,81:10,78" +
",81:5,-1:9,81:5,-1,81:2,80:2,-1:4");

	public TUVienna.CS_CUP.Runtime.Symbol next_token ()
 {
		int yy_lookahead;
		int yy_anchor = YY_NO_ANCHOR;
		int yy_state = yy_state_dtrans[yy_lexical_state];
		int yy_next_state = YY_NO_STATE;
		int yy_last_accept_state = YY_NO_STATE;
		bool yy_initial = true;
		int yy_this_accept;

		yy_mark_start();
		yy_this_accept = yy_acpt[yy_state];
		if (YY_NOT_ACCEPT != yy_this_accept) {
			yy_last_accept_state = yy_state;
			yy_mark_end();
		}
		while (true) {
			if (yy_initial && yy_at_bol) yy_lookahead = YY_BOL;
			else yy_lookahead = yy_advance();
			yy_next_state = YY_F;
			yy_next_state = yy_nxt[yy_rmap[yy_state]][yy_cmap[yy_lookahead]];
			if (YY_EOF == yy_lookahead && true == yy_initial) {
				return null;
			}
			if (YY_F != yy_next_state) {
				yy_state = yy_next_state;
				yy_initial = false;
				yy_this_accept = yy_acpt[yy_state];
				if (YY_NOT_ACCEPT != yy_this_accept) {
					yy_last_accept_state = yy_state;
					yy_mark_end();
				}
			}
			else {
				if (YY_NO_STATE == yy_last_accept_state) {
					throw (new System.Exception("Lexical Error: Unmatched Input."));
				}
				else {
					yy_anchor = yy_acpt[yy_last_accept_state];
					if (0 != (YY_END & yy_anchor)) {
						yy_move_end();
					}
					yy_to_mark();
					switch (yy_last_accept_state) {
					case 1:
						break;
					case -2:
						break;
					case 2:
						{ return new Symbol(Sym.SEMI); }
					case -3:
						break;
					case 3:
						{ return new Symbol(Sym.PLUS); }
					case -4:
						break;
					case 4:
						{ System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
					case -5:
						break;
					case 5:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -6:
						break;
					case 6:
						{ return new Symbol(Sym.RBRACKET); }
					case -7:
						break;
					case 7:
						{ return new Symbol(Sym.LBRACKET); }
					case -8:
						break;
					case 8:
						{ return new Symbol(Sym.EQUAL); }
					case -9:
						break;
					case 9:
						{ return new Symbol(Sym.LPAREN); }
					case -10:
						break;
					case 10:
						{ return new Symbol(Sym.RPAREN); }
					case -11:
						break;
					case 11:
						{ return new Symbol(Sym.COLON); }
					case -12:
						break;
					case 12:
						{ return new Symbol(Sym.QUESTION); }
					case -13:
						break;
					case 13:
						{ return new Symbol(Sym.COMMA); }
					case -14:
						break;
					case 14:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -15:
						break;
					case 15:
						{ /* ignore white space. */break; }
					case -16:
						break;
					case 16:
						{ return (new Symbol(Sym.NUMBER,yytext()));}
					case -17:
						break;
					case 17:
						{ return new Symbol(Sym.IMPLIES); }
					case -18:
						break;
					case 18:
						{ return new Symbol(Sym.TO); }
					case -19:
						break;
					case 19:
						{ return new Symbol(Sym.PROVISO); }
					case -20:
						break;
					case 20:
						{ return new Symbol(Sym.CONDITIONAL); }
					case -21:
						break;
					case 21:
						{ yybegin(COMMENT); comment_count = comment_count + 1;break; }
					case -22:
						break;
					case 22:
						{ return new Symbol(Sym.FIX); }
					case -23:
						break;
					case 23:
						{ return new Symbol(Sym.TRUE); }
					case -24:
						break;
					case 24:
						{ return new Symbol(Sym.SAID); }
					case -25:
						break;
					case 25:
						{ return new Symbol(Sym.FROM); }
					case -26:
						break;
					case 26:
						{ return new Symbol(Sym.KNOWS); }
					case -27:
						break;
					case 27:
						{ return new Symbol(Sym.IMPLIED); }
					case -28:
						break;
					case 28:
						{ return new Symbol(Sym.FUNCTION); }
					case -29:
						break;
					case 29:
						{ return new Symbol(Sym.PRINCIPAL); }
					case -30:
						break;
					case 30:
						break;
					case -31:
						break;
					case 31:
						break;
					case -32:
						break;
					case 32:
						{ comment_count = comment_count + 1;break; }
					case -33:
						break;
					case 33:
						{ 
	comment_count = comment_count - 1; 
	if (comment_count > 0) {
		throw (new System.Exception("Error: bad commented line"));
	}
	if (comment_count == 0) {
    		yybegin(YYINITIAL);
	}
	break;
}
					case -34:
						break;
					case 35:
						{ System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
					case -35:
						break;
					case 36:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -36:
						break;
					case 37:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -37:
						break;
					case 38:
						break;
					case -38:
						break;
					case 40:
						{ System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
					case -39:
						break;
					case 41:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -40:
						break;
					case 42:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -41:
						break;
					case 43:
						break;
					case -42:
						break;
					case 44:
						{ System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
					case -43:
						break;
					case 45:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -44:
						break;
					case 46:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -45:
						break;
					case 47:
						break;
					case -46:
						break;
					case 48:
						{ System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
					case -47:
						break;
					case 49:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -48:
						break;
					case 50:
						{ System.Console.Error.WriteLine("Illegal character: " + yytext() + " found in line " + yyline);break; }
					case -49:
						break;
					case 51:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -50:
						break;
					case 52:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -51:
						break;
					case 53:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -52:
						break;
					case 54:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -53:
						break;
					case 55:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -54:
						break;
					case 56:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -55:
						break;
					case 57:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -56:
						break;
					case 58:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -57:
						break;
					case 59:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -58:
						break;
					case 60:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -59:
						break;
					case 61:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -60:
						break;
					case 62:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -61:
						break;
					case 63:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -62:
						break;
					case 64:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -63:
						break;
					case 65:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -64:
						break;
					case 66:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -65:
						break;
					case 67:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -66:
						break;
					case 68:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -67:
						break;
					case 69:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -68:
						break;
					case 70:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -69:
						break;
					case 71:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -70:
						break;
					case 72:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -71:
						break;
					case 73:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -72:
						break;
					case 74:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -73:
						break;
					case 75:
						{ return (new Symbol(Sym.TOKEN,yytext()));}
					case -74:
						break;
					case 76:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -75:
						break;
					case 77:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -76:
						break;
					case 78:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -77:
						break;
					case 79:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -78:
						break;
					case 80:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -79:
						break;
					case 81:
						{ return (new Symbol(Sym.VARIABLE,yytext()));}
					case -80:
						break;
					default:
						yy_error(YY_E_INTERNAL,false);break;
					}
					yy_initial = true;
					yy_state = yy_state_dtrans[yy_lexical_state];
					yy_next_state = YY_NO_STATE;
					yy_last_accept_state = YY_NO_STATE;
					yy_mark_start();
					yy_this_accept = yy_acpt[yy_state];
					if (YY_NOT_ACCEPT != yy_this_accept) {
						yy_last_accept_state = yy_state;
						yy_mark_end();
					}
				}
			}
		}
	}
}
