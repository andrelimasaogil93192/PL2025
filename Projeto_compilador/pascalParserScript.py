import ply.yacc as yacc
from lexer import tokens, lexer
import sys, os

label_counter = {
    "if": 0,
    "else": 0,
    "while": 0,
    "for": 0,
    "endfor": 0,
    "endif": 0,
    "endw": 0
}

def new_label(prefix):
    label_counter[prefix] += 1
    return f"{prefix}_{label_counter[prefix]}"

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQUAL', 'NEQ', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIV_OP', 'MOD_OP'),
    ('right', 'NOT'),
)

def p_programa(p):
    'programa : PROGRAM ID SEMI declaracoes bloco DOT'
    p[0] = p[4] + p[5]

def p_declaracoes(p):
    '''declaracoes : VAR lista_decl_var
                   | empty'''
    if len(p) == 3:
        p[0] = []

def p_lista_decl_var(p):
    '''lista_decl_var : decl_var SEMI lista_decl_var
                      | decl_var SEMI'''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_decl_var(p):
    '''decl_var : lista_id COLON tipo
                | ID COLON tipo
                | ID COLON ARRAY LBRACKET NUMBER DOTDOT NUMBER RBRACKET OF tipo'''
    if len(p) == 4:
        nomes = p[1] if isinstance(p[1], list) else [p[1]]
        tipo = p[3]
        p[0] = (nomes, tipo)
    else:
        nome = p[1]
        tipo = p[10]
        inicio = p[5]
        fim = p[7]
        nomes = [f"{nome}[{i}]" for i in range(inicio, fim + 1)]
        p[0] = (nomes, tipo)

def p_lista_id(p):
    '''lista_id : ID
                | ID COMMA lista_id'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_tipo(p):
    '''tipo : INTEGER
            | BOOLEAN
            | STRING_TYPE'''
    p[0] = p[1]

def p_bloco(p):
    'bloco : BEGIN lista_comandos END'
    p[0] = p[2]

def p_lista_comandos(p):
    '''lista_comandos : comando
                      | comando SEMI lista_comandos'''
    if len(p) == 2:
        p[0] = p[1] if isinstance(p[1], list) else [p[1]]
    else:
        p[0] = (p[1] if isinstance(p[1], list) else [p[1]]) + p[3]

def p_comando(p):
    '''comando : atribuicao
               | leitura
               | escrita
               | para
               | condicional
               | repeticao
               | bloco
               | empty'''
    p[0] = p[1]

def p_atribuicao(p):
    'atribuicao : ID ASSIGN expr'
    p[0] = p[3] + [f'STORE {p[1]}']

def p_atribuicao_array(p):
    'atribuicao : ID LBRACKET ID RBRACKET ASSIGN expr'
    p[0] = p[6] + [f'STORE {p[1]}[{p[3]}]']

def p_leitura(p):
    '''leitura : READLN LPAREN ID RPAREN
               | READLN LPAREN ID LBRACKET ID RBRACKET RPAREN'''
    if len(p) == 5:
        p[0] = [f'READ {p[3]}']
    else:
        p[0] = [f'READ {p[3]}[{p[5]}]']

def p_escrita(p):
    'escrita : WRITELN LPAREN argumentos RPAREN'
    p[0] = p[3] + ['PRINT']

def p_argumentos(p):
    '''argumentos : argumento
                  | argumento COMMA argumentos'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[3]

def p_argumento(p):
    '''argumento : expr
                 | STRING'''
    if isinstance(p[1], str) and p.slice[1].type == "STRING":
        p[0] = [f'PUSH "{p[1]}"']
    else:
        p[0] = p[1]

def p_condicional(p):
    '''condicional : IF expr_bool THEN comando
                   | IF expr_bool THEN comando ELSE comando'''
    endif = new_label("endif")
    if len(p) == 5:
        p[0] = p[2] + [f'JZ {endif}'] + p[4] + [f'{endif}:']
    else:
        elsel = new_label("else")
        p[0] = p[2] + [f'JZ {elsel}'] + p[4] + [f'JMP {endif}', f'{elsel}:'] + p[6] + [f'{endif}:']

def p_repeticao(p):
    'repeticao : WHILE expr_bool DO comando'
    wl = new_label("while")
    endw = new_label("endw")
    p[0] = [f'{wl}:'] + p[2] + [f'JZ {endw}'] + p[4] + [f'JMP {wl}', f'{endw}:']

def p_para(p):
    '''para : FOR ID ASSIGN expr TO expr DO comando
            | FOR ID ASSIGN expr DOWNTO expr DO comando'''
    loop = new_label("for")
    end = new_label("endfor")
    p[0] = p[4] + [f'STORE {p[2]}']
    p[0] += [f'{loop}:', f'LOAD {p[2]}'] + p[6]
    if p[5] == 'to':
        p[0] += ['GT', f'JZ {end}']
        corpo = p[8]
        p[0] += corpo + [f'LOAD {p[2]}', 'PUSH 1', 'ADD', f'STORE {p[2]}']
    else:
        p[0] += ['LT', f'JZ {end}']
        corpo = p[8]
        p[0] += corpo + [f'LOAD {p[2]}', 'PUSH 1', 'SUB', f'STORE {p[2]}']
    p[0] += [f'JMP {loop}', f'{end}:']

def p_expr_bool(p):
    '''expr_bool : expr
                 | expr op_rel expr
                 | expr_bool AND expr_bool
                 | expr_bool OR expr_bool'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] == 'and':
        p[0] = p[1] + p[3] + ['AND']
    elif p[2] == 'or':
        p[0] = p[1] + p[3] + ['OR']
    else:
        p[0] = p[1] + p[3] + [p[2]]

def p_op_rel(p):
    '''op_rel : EQUAL
              | NEQ
              | LT
              | LE
              | GT
              | GE'''
    p[0] = {'=': 'EQ', '<>': 'NE', '<': 'LT', '<=': 'LE', '>': 'GT', '>=': 'GE'}[p[1]]

def p_expr(p):
    '''expr : termo
            | expr PLUS termo
            | expr MINUS termo'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[3] + ['ADD' if p[2] == '+' else 'SUB']

def p_termo(p):
    '''termo : fator
             | termo TIMES fator
             | termo DIV_OP fator
             | termo MOD_OP fator'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        op = {'*': 'MUL', 'div': 'DIV', 'mod': 'MOD'}[p[2]]
        p[0] = p[1] + p[3] + [op]

def p_fator(p):
    '''fator : NUMBER
             | STRING
             | TRUE
             | FALSE
             | ID
             | ID LBRACKET ID RBRACKET
             | LPAREN expr_bool RPAREN
             | NOT fator
             | LENGTH LPAREN ID RPAREN'''  
    if len(p) == 2:
        if isinstance(p[1], int):
            p[0] = [f'PUSH {p[1]}']
        elif p[1] == 'true':
            p[0] = ['PUSH 1']
        elif p[1] == 'false':
            p[0] = ['PUSH 0']
        elif p.slice[1].type == "STRING":
            p[0] = [f'PUSH "{p[1]}"']
        else:
            p[0] = [f'LOAD {p[1]}']
    elif len(p) == 4:
        p[0] = p[2]
    elif len(p) == 5:
        if p[1] == 'length':
            p[0] = [f'LENGTH {p[3]}']
        else:
            p[0] = [f'LOAD {p[1]}[{p[3]}]']
    else:
        p[0] = p[2] + ['NOT']


def p_fator_funcao(p):
    'fator : ID LPAREN ID RPAREN'
    p[0] = [f'{p[1].upper()} {p[3]}']

def p_empty(p):
    'empty :'
    p[0] = []

def p_error(p):
    if p:
        print(f"Erro de sintaxe próximo a '{p.value}', linha {p.lineno}")
    else:
        print("Erro de sintaxe no final do ficheiro")

parser = yacc.yacc()





if __name__ == "__main__":
    import sys, os
    if len(sys.argv) != 2:
        print("Uso: python parser.py <ficheiro.pas>")
        sys.exit(1)

    file_path = sys.argv[1]
    with open(file_path, "r", encoding="utf-8") as f:
        source = f.read()

    print("Tokens reconhecidos:")
    lexer.input(source)
    for tok in lexer:
        print(tok)

    lexer.input(source)
    result = parser.parse(source, lexer=lexer)

    if result is None:
        print("Erro: o parser devolveu None.")
        sys.exit(1)

    output_path = os.path.splitext(file_path)[0] + ".vm"
    with open(output_path, "w") as f:
        for line in result:
            f.write(line + "\n")

    print(f"Código VM gerado com sucesso em: {output_path}")
