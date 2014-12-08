#include "lexer.h"

bool isSymbolStart(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_';}
bool isSymbolCont(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || (c>='0' && c<='9');}
bool isNumStart(char c){return (c>='0'&&c<='9');};
bool isNum(char c){return (c>='0'&&c<='9')||(c>='a'&&c<='f')||(c=='e')||(c=='x') ||c=='.';};
bool isWhitespace(char c){return  c==' '||c=='\n'||c=='\a'||c=='\t';};
bool isOperator(char c){return c=='+'||c=='-'||c=='*'||c=='/'||c=='.'||c=='='||c=='>'||c=='<'||c=='&'||c=='|'||c=='~'||c=='%'||c=='^'||c=='+';}

