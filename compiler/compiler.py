#!/usr/bin/python
import struct, sys

# operators in precedence order
INFIX_LEVEL = {
    "=":   0,
    "|":   1,
    "^":   2,
    "&":   3,
    "==":  4,
    "!=":  5,
    "<":   7,
    ">":   7,
    "<=":  9,
    ">=":  9,
    "<<": 11,
    ">>": 11,
    "+":  13,
    "-":  13,
    "*":  16,
    "/":  16,
    "%":  16,
}

STMT_TYPES = [
    "block",
    "return",
]

EXPR_TYPES = INFIX_LEVEL.keys() + [
    "number",
    "neg",
    "not",
    "call",
    "use",
]


class Node:
    pos = (0, 0)
    def __init__(self, type):
        self.type = type
        self.kids = []
        self.parent = None

    def add(self, kid):
        kid.parent = self
        self.kids.append(kid)
        return self

    def pretty_print(self, indent=0):
        line = " " * indent + self.type
        if hasattr(self, "name"):   line += " %r" % self.name
        if hasattr(self, "number"): line += " %d" % self.number
        print line
        for k in self.kids:
            k.pretty_print(indent + 1)

    def is_lvalue(self):
        return self.type == "use"

    def is_stmt(self): return self.type in STMT_TYPES
    def is_expr(self): return self.type in EXPR_TYPES


KEYWORDS = [
    "return",
    "if",
    "else",
    "while",
    "for",
    "break",
    "continue",
    "enum",
]


def error(node, msg = ""):
    print "error at %d:%d: " % node.pos + msg
    exit(1)


class Token:
    def __str__(self):
        return repr(self.__dict__)


class Lexer:
#public:
    def __init__(self, file):
        self.file = file
        self.row  = 1
        self.col  = 0
        self.char = None
        self.next_char()

    def next_token(self):
        self.token.type = self.scan()
        return self.token

#private:
    def next_char(self):
        c = self.char
        self.char = self.file.read(1)
        if self.char == "\n":
            self.row += 1
            self.col = 0
        elif self.char:
            self.col += 1
        return c

    def scan(self):
        while self.char:
            token = Token()
            token.pos = (self.row, self.col)
            self.token = token

            c = self.next_char()

            if c.isalpha() or c == '_':
                token.name = c
                while self.char.isalnum() or self.char == '_':
                    token.name += self.next_char()
                if token.name in KEYWORDS: return token.name
                return "id"

            if c.isdigit():
                token.number = int(c)
                while self.char.isdigit():
                    token.number = token.number * 10 + int(self.next_char())
                return "number"

            if c in "+-*%(){};~,": return c
            if c == "/":
                if self.char != '/': return "/";
                while self.char and self.char != "\n": self.next_char()
                continue

            if c in "<>":
                if self.char not in c + "=": return c
                return c + self.next_char()

            if c in "!=":
                if self.char != "=": return c
                return c + self.next_char()

            if not c.isspace():
                error(self.token, "unexpected character %r" % c)

        return None



class Parser:
    def __init__(self, file):
        self.lexer = Lexer(file)
        self.token = None

    def next_token(self):
        t = self.token
        self.token = self.lexer.next_token()
        return t

    def match_token(self, type):
        if type != self.token.type:
            error(self.token, "unexpected token %r (should probably be %r)" % (self.token.type, type))
        return self.next_token()

    def parse(self):

        self.enum = {}
        prog = Node("program")

        self.next_token()

        while self.token.type:

            if self.token.type == "enum":
                self.next_token()
                self.match_token("{")
                i = 0
                while self.token.type != "}":
                    n = self.token.name
                    self.match_token("id")
                    if self.token.type == "=":
                        self.next_token()
                        i = self.match_token("number").number
                    self.enum[n] = i
                    i += 1
                    if self.token.type == ",":
                        self.next_token()
                        continue
                    break
                self.match_token("}")
                self.match_token(";")


            # read type and name
            t = self.match_token("id")
            n = self.match_token("id")

            # function
            if self.token.type == "(":
                for k in prog.kids:
                    if k.type == "func" and k.name == n.name:
                        error(n, "redefinition of function %r" % n.name)
                self.next_token()
                node = Node("func")
                if t.name not in ("void", "int"): error(t, "invalid type")
                node.t = t.name
                node.name = n.name
                # TODO: arguments
                self.match_token(")")
                if self.token.type != "{": error(self.token, "no function block")

                node.add(self.stmt())
                prog.add(node)
                continue

            # variable
            if t.name not in ("int"): error(t, "invalid type")
            for k in prog.kids:
                if k.type == "var" and k.name == n.name:
                    error(n, "redefinition of variable %r" % n.name)
            node = Node("var")
            node.t = t.name
            node.name = n.name
            self.match_token(";")
            prog.add(node)
            continue

        return prog


    def stmt(self):

        if self.token.type == "return":
            node = Node("return")
            node.pos = self.token.pos
            self.next_token()
            if self.token.type != ";": node.add(self.expr())
            self.match_token(";")
            return node

        elif self.token.type == "for":
            self.next_token()
            # fake for loop by using a while loop
            node = Node("block")
            self.match_token("(")
            if self.token.type != ";":
                node.add(self.expr())
            self.match_token(";")
            loop = Node("while")
            node.add(loop)
            if self.token.type != ";":
                cond = self.expr()
            else:
                cond = Node("number")
                cond.number = 1
            self.match_token(";")
            loop.add(cond)
            if self.token.type != ")":
                inc = self.expr()
            else:
                inc = None
            self.match_token(")")
            body = Node("block")
            loop.add(body)
            body.add(self.stmt())
            if inc: body.add(inc)
            return node

        elif self.token.type == "while":
            self.next_token()
            node = Node("while")
            self.match_token("(")
            node.add(self.expr())
            self.match_token(")")
            node.add(self.stmt())
            return node

        elif self.token.type == "if":
            self.next_token()
            node = Node("if")
            self.match_token("(")
            node.add(self.expr())
            self.match_token(")")
            node.add(self.stmt())
            if self.token.type == "else":
                self.next_token()
                node.add(self.stmt())
            return node

        elif self.token.type == "{":
            self.next_token()
            node = Node("block")
            while self.token.type != "}": node.add(self.stmt())
            self.next_token()
            return node

        else:
            node = self.expr()
            self.match_token(";")
            return node

    def expr(self, level=0):

        # unary stuff
        if self.token.type == "number":
            node = Node("number")
            node.number = self.next_token().number
        elif self.token.type == "+":
            self.next_token()
            node = self.expr(99)
        elif self.token.type == "-":
            self.next_token()
            node = Node("neg").add(self.expr(99))
        elif self.token.type == "(":
            self.next_token()
            node = self.expr()
            self.match_token(")")
        elif self.token.type == "!":
            self.next_token()
            node = Node("not").add(self.expr(99))
        elif self.token.type == "id":
            t = self.next_token()
            if self.token.type == "(":
                self.next_token()
                node = Node("call")
                node.name = t.name
                node.pos = t.pos
                # TODO: args
                self.match_token(")")
            else:
                if t.name in self.enum:
                    node = Node("number")
                    node.number = self.enum[t.name]
                else:
                    node = Node("use")
                    node.pos = t.pos
                    node.name = t.name
        else:
            error(self.token, "bad expression %r" % self.token.type)

        # infix stuff
        while self.token.type in INFIX_LEVEL and INFIX_LEVEL[self.token.type] >= level:
            tok = self.token.type
            self.next_token()
            node = Node(tok).add(node).add(self.expr(INFIX_LEVEL[tok] + 1))
        return node



def resolve_names(node):
    for k in node.kids: resolve_names(k)

    if node.type == "use":
        n = node
        while n.parent:
            kid = n
            n = n.parent
        for k in n.kids:
            # we have not found the variable declaration prior to its use
            if k == kid:
                error(node, "use of undefined variable %r" % node.name)
            if k.type == "var" and k.name == node.name:
                node.var = k
                break

    elif node.type == "call":
        n = node
        while n.parent: n = n.parent
        for k in n.kids:
            if k.type == "func" and k.name == node.name:
                node.func = k
                break
        else:
            error(node, "call of undefined function %r" % node.name)


def check_types(node):
    for k in node.kids: check_types(k)

    if node.type == "return":
        t = node.kids[0].t if node.kids else "void"
        n = node.parent
        while n.type != "func": n = n.parent
        if n.t != t: error(node, "return type mismatch")
        # TODO: check for missing return statement somehow

    if not node.is_expr(): return
    if   node.type == "number": node.t = "int"
    elif node.type == "use":    node.t = node.var.t
    elif node.type == "call":   node.t = node.func.t
    else:
        node.t = node.kids[0].t
        # all children must have the same expression type
        if any(k.t != node.t for k in node.kids): error(node, "type mismatch")
        if node.t == "void": error(node, "type mismatch")


def fold_constants(node):
    for k in node.kids: fold_constants(k)
    if not all(k.type == "number" for k in node.kids): return
    if node.type in ("+", "-", "*", "/", "%"):
        node.number = eval("%d %s %d" % (node.kids[0].number, node.type, node.kids[1].number))
        node.type = "number"
        node.kids = []
    elif node.type == "neg":
        node.number = -node.kids[0].number
        node.type = "number"
        node.kids = []
    elif node.type == "not":
        node.number = 0 if node.kids[0].number else 1
        node.type = "number"
        node.kids = []

    # TODO: handle other operators


OPCODES = [
    "IMM", "LI", "SI", "PUSH",
    "JMP", "JZ", "JNZ", "CALL", "RET",
    "INC", "DEC", "NOT", "NEG",
    #"OR", "XOR", "AND", "EQ", "NE", "LT", "LE", "GT", "GE", "SHL", "SHR", "ADD", "SUB", "MUL", "DIV", "MOD",
    "|", "^", "&", "==", "!=", "<", "<=", ">", ">=", "<<", ">>", "+", "-", "*", "/", "%",
]

OPCODES_ = [
    "IMM", "LI", "SI", "PUSH",
    "JMP", "JZ", "JNZ", "CALL", "RET",
    "INC", "DEC", "NOT", "NEG",
    "OR", "XOR", "AND", "EQ", "NE", "LT", "LE", "GT", "GE", "SHL", "SHR", "ADD", "SUB", "MUL", "DIV", "MOD",
    #"|", "^", "&", "==", "!=", "<", "<=", ">", ">=", "<<", ">>", "+", "-", "*", "/", "%",
]



OT = dict((o, i) for i, o in enumerate(OPCODES))


def compile(prog):
    assert prog.type == "program"

    var_offset = 0
    bytecode = []
    def emit(b):
        if type(b) == int: bytecode.append(b)
        else: bytecode.append(OT[b])

    linking = []

    def cmpl(node):
        if node.type == "func":
            node.address = len(bytecode)
            blk = node.kids[0]
            cmpl(blk)
            if not blk.kids or blk.kids[-1].type != "return": emit("RET")

        elif node.type == "block":
            for k in node.kids: cmpl(k)

        elif node.type == "return":
            if node.kids: cmpl(node.kids[0])
            emit("RET")

        elif node.type == "while":
            s = len(bytecode)
            if node.kids[0].type == "number" and node.kids[0].number != 0:
                # condition is always true
                a = None
            else:
                cmpl(node.kids[0])
                emit("JZ")
                a = len(bytecode)
                emit(0)
            cmpl(node.kids[1])
            emit("JMP")
            emit(s)
            if a:
                bytecode[a] = len(bytecode)

        elif node.type == "if":
            cmpl(node.kids[0])
            emit("JZ")
            a = len(bytecode)
            emit(0)
            cmpl(node.kids[1])
            # else
            if len(node.kids) == 3:
                bytecode[a] = len(bytecode) + 2
                emit("JMP")
                a = len(bytecode)
                emit(0)
                cmpl(node.kids[2])
            bytecode[a] = len(bytecode)

        elif node.type == "use":
            emit("IMM")
            emit(node.var.address)
            emit("LI")

        elif node.type == "call":
            emit("CALL")
            linking.append((node.func, len(bytecode)))
            emit(0)

        elif node.type == "=":
            # lvalue check for node.kids[0]
            if not node.kids[0].is_lvalue():
                error(node, "assignment destination must be lvalue")
            cmpl(node.kids[0])
            bytecode.pop()
            emit("PUSH")
            cmpl(node.kids[1])
            emit("SI")

        elif node.type == "number":
            emit("IMM")
            emit(node.number)

        elif node.type == "neg":
            cmpl(node.kids[0])
            emit("NEG")

        elif node.type == "not":
            cmpl(node.kids[0])
            emit("NOT")

        elif node.type in OPCODES:
            cmpl(node.kids[0])
            emit("PUSH")
            cmpl(node.kids[1])
            emit(OPCODES.index(node.type))

        else:
            assert False, "TODO: compile node type %r" % node.type

    for k in prog.kids:
        if k.type == "var":
            k.address = var_offset
            var_offset += 1
        elif k.type == "func":
            cmpl(k)
        else:
            assert False

    for n, a in linking:
        bytecode[a] = n.address

    return bytecode


if __name__ == "__main__":
    ast = Parser(open(sys.argv[1])).parse()
    resolve_names(ast)
    check_types(ast)
    fold_constants(ast)
    ast.pretty_print()

    bytecode = compile(ast)
    file("bytecode", "w").write("".join(struct.pack("@i", x) for x in bytecode))

    i = 0
    while i < len(bytecode):
        b = OPCODES_[bytecode[i]]
        print "%3d\t%s\t" % (i, b),
        i += 1
        if b in ["JMP", "JZ", "JNZ", "IMM"]:
            print bytecode[i],
            i += 1
        print
