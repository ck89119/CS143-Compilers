/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;


/*
 *  Add Your own definitions here
 */

int comment_level = 0;

%}

/*
 * Define names for regular expressions here.
 */

DARROW =>
DIGIT [0-9]
%Start COMMENT
%Start INLINE_COMMENT
%Start STRING
%option noyywrap

%%

 /*
  *  Nested comments
  */
<INITIAL>"*)" {
  yylval.error_msg = "Unmatched *)";
  return ERROR;
}

<INITIAL,COMMENT>"(*" {
  comment_level += 1;
  BEGIN COMMENT;
}

<COMMENT>"*)" {
  if (--comment_level == 0) {
    BEGIN INITIAL;
  }
}

<COMMENT><<EOF>> {
  yylval.error_msg = "EOF in comment";
  BEGIN INITIAL;
  return ERROR;
}

<COMMENT>\n { curr_lineno += 1; }
<COMMENT>[^\n(*]* {}
<COMMENT>[(*] {}

 /* INLINE_COMMENT */
<INITIAL>"--" {
  BEGIN INLINE_COMMENT;
}

<INLINE_COMMENT>\n {
  curr_lineno += 1;
  BEGIN INITIAL;
}

<INLINE_COMMENT>[^\n]* {}
 
 /* Keywords */
(?i:class) { return CLASS; }
(?i:else) { return ELSE; }
(?i:fi) { return FI; }
(?i:if) { return IF; }
(?i:in) { return IN; }
(?i:inherits) { return INHERITS; }
(?i:let) { return LET; }
(?i:loop) { return LOOP; }
(?i:pool) { return POOL; }
(?i:then) { return THEN; }
(?i:while) { return WHILE; }
(?i:case) { return CASE; }
(?i:esac) { return ESAC; }
(?i:of) { return OF; }
(?i:new) { return NEW; }
(?i:isvoid) { return ISVOID; }
(?i:not) { return NOT; }

 /* STR_CONST */
<INITIAL>\" {
  BEGIN STRING;
  yymore();
}

<STRING>\n {
  yylval.error_msg = "Unterminated string constant";
  BEGIN INITIAL;
  curr_lineno += 1;
  return ERROR;
}

<STRING><<EOF>> {
  yylval.error_msg = "EOF in string constant";
  BEGIN INITIAL;
  yyrestart(yyin);
  return ERROR;
}

<STRING>[^\\\"\n]* { yymore(); }
<STRING>\\[^\n] { yymore(); }
<STRING>\\\n {
  curr_lineno += 1;
  yymore();
}

<STRING>\" {
  std::string input(yytext, yyleng);
  std::string output = "";
  std::string::size_type pos = 0;

  input = input.substr(1, input.size() - 2);

  if (input.find_first_of('\0') != std::string::npos) {
    yylval.error_msg = "String contains null character";
    BEGIN INITIAL;
    return ERROR;
  }

  while (pos = input.find_first_of('\\'), pos != std::string::npos) {
    output += input.substr(0, pos);
    if (input[pos+1] == 'b') output += '\b';
    else if (input[pos+1] == 't') output += '\t';
    else if (input[pos+1] == 'n') output += '\n';
    else if (input[pos+1] == 'f') output += '\f';
    else output += input[pos+1];

    input = input.substr(pos + 2);
  }
  output += input;

  if (output.size() > 1024) {
    yylval.error_msg = "String constant too long";
    BEGIN INITIAL;
    return ERROR;
  }

  yylval.symbol = stringtable.add_string((char*)output.c_str());
  BEGIN INITIAL;
  return STR_CONST;
}

 /* INT_CONST */
{DIGIT}+ {
  yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}

 /* BOOL_CONST */
f(?i:alse) { 
  yylval.boolean = 0;
  return BOOL_CONST; 
}
t(?i:rue) {
  yylval.boolean = 1;
  return BOOL_CONST;
}

 /* TYPEID */
[A-Z][a-zA-Z0-9_]* { 
  yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

 /* OBJECTID */
[a-z][a-zA-Z0-9_]* {
  yylval.symbol = idtable.add_string(yytext);
  return OBJECTID; 
}

 /* white space */
[ \f\r\t\v] {}

 /* line */
[\n] { curr_lineno += 1; }

 /* operators */
{DARROW} { return (DARROW); }
"<-" { return ASSIGN; }
"<=" { return LE; }
"+" { return int('+'); }
"-" { return int('-'); }
"*" { return int('*'); }
"/" { return int('/'); }
"<" { return int('<'); }
"=" { return int('='); }
"." { return int('.'); }
";" { return int(';'); }
"~" { return int('~'); }
"{" { return int('{'); }
"}" { return int('}'); }
"(" { return int('('); }
")" { return int(')'); }
":" { return int(':'); }
"@" { return int('@'); }
"," { return int(','); }

 /* error */
. {
  yylval.error_msg = yytext;
  return ERROR;
}

%%
