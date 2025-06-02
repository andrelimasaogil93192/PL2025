
import ply.lex as lex

# Lista de tokens
tokens = [
    'ID', 'NUMBER', 'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'ASSIGN', 'EQUAL', 'NEQ', 'LT', 'LE', 'GT', 'GE',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET',
    'SEMI', 'COLON', 'COMMA', 'DOT', 'DOTDOT','LENGTH','DOWNTO'
]

reserved = {
    'program': 'PROGRAM',
    'begin': 'BEGIN',
    'end': 'END',
    'var': 'VAR',
    'integer': 'INTEGER',
    'boolean': 'BOOLEAN',
    'string': 'STRING_TYPE',
    'readln': 'READLN',
    'writeln': 'WRITELN',
    'write': 'WRITE',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'for': 'FOR',
    'to': 'TO',
    'function': 'FUNCTION',
    'true': 'TRUE',
    'false': 'FALSE',
    'div': 'DIV_OP',
    'mod': 'MOD_OP',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',
    'array': 'ARRAY',
    'of': 'OF',
    'downto': 'DOWNTO',
    'length': 'LENGTH',
}

tokens += list(reserved.values())

# Expressões regulares para os tokens   
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_ASSIGN = r':='
t_EQUAL = r'='
t_NEQ = r'<>'
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_SEMI = r';'
t_COLON = r':'
t_COMMA = r','
t_DOT = r'\.'
t_DOTDOT = r'\.\.'

t_ignore = ' \t'

def t_COMMENT(t):
    r'\{[^}]*\}'
    pass

def t_STRING(t):
    r"'[^']*'"
    t.value = t.value[1:-1]
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value.lower(), 'ID')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Caractere ilegal: {t.value[0]}")
    t.lexer.skip(1)

lexer = lex.lex()