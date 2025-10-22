from typing import List, Union, TypedDict, Optional
import cmath
import math



class CalcStep(TypedDict):
    op:      callable  # 操作符 callable
    args:    List[Union[complex, str]]  # 参数列表 支持变量名 str 或数值 complex
    res:     complex  # 计算结果 complex
    res_ptr: int  # 结果存储位置(-1为输出) int

class Calcflow:
    def __init__(self,flow: List[CalcStep]):
        self.calcflow_base = {
            "op": None,  # 操作符 callable
            "args": [],  # 参数列表 List[complex]
            "res": 0,  # 计算结果 complex
            "res_ptr": -1,  # 结果存储位置(-1为输出) int
        }
        self.flow:    List[CalcStep] = flow or []  # 计算流
        self.idx:     int            = 0  # 当前计算位置
        # 安全处理空流程
        if len(self.flow) == 0:
            self.op = None
            self.args = None
            self.res = 0+0j
            self.res_ptr = -1
        else:
            self.op:      callable       = self.flow[self.idx]["op"]  # 操作符 callable
            self.args:    List[complex]  = self.flow[self.idx]["args"]  # 参数列表 List[complex]
            self.res:     complex        = self.flow[self.idx]["res"]  # 计算结果 complex
            self.res_ptr: int            = self.flow[self.idx]["res_ptr"]  # 结果存储位置(-1为输出) int


    def next(self):
        # 先把当前步骤的结果推给目标步骤
        if 0 <= self.idx < len(self.flow):
            cur_res_ptr = self.flow[self.idx]["res_ptr"]
            if cur_res_ptr != -1:
                self.flow[cur_res_ptr]["args"].append(self.res)

        # 移动到下一步
        self.idx += 1
        if self.idx < len(self.flow):
            self.op      = self.flow[self.idx]["op"]
            self.args    = self.flow[self.idx]["args"]
            self.res_ptr = self.flow[self.idx]["res_ptr"]
        else:
            self.op      = None
            self.args    = None
            self.res_ptr = None

class Calc:
    def __init__(self, calcflow: List[dict]):
        self.variables = {}
        self.calcflow  = Calcflow(calcflow)
        self.nan = complex(float('nan'), float('nan'))
        self.inf = complex(float('inf'), float('inf'))

    def calculate(self, **variables):
        # 合并传入变量
        if variables:
            for k, v in variables.items():
                self.variables[k] = to_complex(v)
        # 设置变量环境（供 op_var 使用）
        global VAR_ENV
        old_env = VAR_ENV
        VAR_ENV = self.variables
        try:
            while self.calcflow.op is not None:
                self.calcflow.res = self.calcflow.op(*self.calcflow.args)
                self.calcflow.next()
            return self.calcflow.res
        finally:
            VAR_ENV = old_env
    
    # 可在此扩展变量或自定义函数映射


# =========================
# 基本运算与常量映射
# =========================

def op_push(x: complex) -> complex:
    return x

def op_add(*args: complex) -> complex:
    s = 0+0j
    for a in args:
        s += a
    return s

def op_sub(*args: complex) -> complex:
    if len(args) == 0:
        return 0+0j
    if len(args) == 1:
        return -args[0]
    res = args[0]
    for a in args[1:]:
        res -= a
    return res

def op_mul(*args: complex) -> complex:
    res = 1+0j
    for a in args:
        res *= a
    return res

def op_div(*args: complex) -> complex:
    if len(args) == 0:
        return complex(float('nan'))
    if len(args) == 1:
        return 1/args[0]
    res = args[0]
    for a in args[1:]:
        res /= a
    return res

def op_pow(a: complex, b: complex) -> complex:
    return a ** b

def op_log(base: complex, upper: complex) -> complex:
    # 对数 log(base, upper) => log_upper / log_base
    return cmath.log(upper) / cmath.log(base)

def op_neg(x: complex) -> complex:
    return -x

def op_sin(x: complex) -> complex:
    return cmath.sin(x)

def op_cos(x: complex) -> complex:
    return cmath.cos(x)

def op_tan(x: complex) -> complex:
    return cmath.tan(x)

def op_exp(x: complex) -> complex:
    return cmath.exp(x)

def op_abs(x: complex) -> complex:
    return complex(abs(x), 0.0)

def op_var(name: str) -> complex:
    # 变量解析：从当前环境取值
    val = VAR_ENV.get(name)
    if val is None:
        raise NameError(f"变量未定义: {name}")
    return to_complex(val)

def to_complex(v) -> complex:
    if isinstance(v, complex):
        return v
    if isinstance(v, (int, float)):
        return complex(v, 0.0)
    raise TypeError(f"变量值类型不支持: {type(v)}")

VAR_ENV: dict = {}


OP_FUNCS = {
    'push': op_push,
    'add': op_add,
    'sub': op_sub,
    'mul': op_mul,
    'div': op_div,
    'pow': op_pow,
    'log': op_log,
    'neg': op_neg,
    'sin': op_sin,
    'cos': op_cos,
    'tan': op_tan,
    'exp': op_exp,
    'abs': op_abs,
    'var': op_var,
}

CONST_MAP = {
    'pi': complex(math.pi, 0.0),
    'e': complex(math.e, 0.0),
    'i': 0+1j,
}

class Tokenizer:
    # 词法分析器
    # 加法 add(list[complex]) -> complex
    # 减法 sub(list[complex]) -> complex
    # 乘法 mul(list[complex]) -> complex
    # 除法 div(list[complex]) -> complex
    # 幂指开方运算 pow(list[complex]) -> complex
    # 对数 log(base, upper)
    # 三角函数 xxx(number) -> complex
    def __init__(self):
        self.s: str = ''
        self.i: int = 0

    # ============ 词法部分 ============
    class Tok(TypedDict):
        type: str
        value: str

    def _peek(self) -> str:
        return self.s[self.i] if self.i < len(self.s) else ''

    def _next(self) -> str:
        ch = self._peek()
        if ch:
            self.i += 1
        return ch

    def _lex(self, expr: str) -> List['Tokenizer.Tok']:
        self.s = expr
        self.i = 0
        toks: List[Tokenizer.Tok] = []
        while self.i < len(self.s):
            ch = self._peek()
            if ch.isspace():
                self.i += 1
                continue
            if ch.isdigit() or (ch == '.' and self.i+1 < len(self.s) and self.s[self.i+1].isdigit()):
                start = self.i
                self.i += 1
                while self.i < len(self.s) and (self.s[self.i].isdigit() or self.s[self.i] == '.'):
                    self.i += 1
                # 复数虚部后缀 i，例如 4i、0.5i
                if self.i < len(self.s) and self.s[self.i] == 'i':
                    self.i += 1
                    toks.append({'type': 'IMAG', 'value': self.s[start:self.i-1]})
                else:
                    toks.append({'type': 'NUMBER', 'value': self.s[start:self.i]})
                continue
            if ch.isalpha() or ch == '_':
                start = self.i
                self.i += 1
                while self.i < len(self.s) and (self.s[self.i].isalnum() or self.s[self.i] == '_'):
                    self.i += 1
                toks.append({'type': 'IDENT', 'value': self.s[start:self.i]})
                continue
            if ch in '+-*/^(),':
                self.i += 1
                map_type = {
                    '+': 'PLUS', '-': 'MINUS', '*': 'STAR', '/': 'SLASH', '^': 'CARET',
                    '(': 'LPAREN', ')': 'RPAREN', ',': 'COMMA'
                }
                toks.append({'type': map_type[ch], 'value': ch})
                continue
            raise SyntaxError(f'非法字符: {ch}')
        toks.append({'type': 'EOF', 'value': ''})
        return toks

    # ============ 语法部分(Pratt) ============
    def tokenize(self, expr: str) -> List[CalcStep]:
        toks = self._lex(expr)
        pos = 0

        def peek_type() -> str:
            return toks[pos]['type']

        def take(t: Optional[str] = None) -> 'Tokenizer.Tok':
            nonlocal pos
            tok = toks[pos]
            if t and tok['type'] != t:
                raise SyntaxError(f'期望 {t} 但得到 {tok["type"]}')
            pos += 1
            return tok

        # AST 节点采用简单 dict
        def parse_primary():
            tok = toks[pos]
            t = tok['type']
            if t == 'NUMBER':
                take()
                return {'type': 'number', 'value': complex(float(tok['value']), 0.0)}
            if t == 'IMAG':
                take()
                return {'type': 'number', 'value': complex(0.0, float(tok['value']))}
            if t == 'IDENT':
                ident = take()['value']
                # 函数调用 IDENT(
                if peek_type() == 'LPAREN':
                    take('LPAREN')
                    args = []
                    if peek_type() != 'RPAREN':
                        while True:
                            args.append(parse_expr(0))
                            if peek_type() == 'COMMA':
                                take('COMMA')
                                continue
                            break
                    take('RPAREN')
                    return {'type': 'call', 'name': ident, 'args': args}
                # 常量
                if ident in CONST_MAP:
                    return {'type': 'const', 'name': ident}
                # 变量
                return {'type': 'var', 'name': ident}
            if t == 'LPAREN':
                take('LPAREN')
                node = parse_expr(0)
                take('RPAREN')
                return node
            if t == 'MINUS':
                # 一元负号
                take('MINUS')
                operand = parse_expr(100)  # 高绑定力处理为前缀
                return {'type': 'unary', 'op': 'neg', 'arg': operand}
            if t == 'PLUS':
                take('PLUS')
                return parse_expr(100)
            raise SyntaxError(f'无法解析的起始符: {t}')

        # 操作符优先级与结合性
        BP = {
            'PLUS': (10, 'L', 'add'),
            'MINUS': (10, 'L', 'sub'),
            'STAR': (20, 'L', 'mul'),
            'SLASH': (20, 'L', 'div'),
            'CARET': (30, 'R', 'pow'),
        }

        def parse_expr(min_bp: int):
            left = parse_primary()
            while True:
                t = peek_type()
                if t not in BP:
                    break
                bp, assoc, opname = BP[t]
                if assoc == 'L' and bp < min_bp:
                    break
                if assoc == 'R' and bp <= min_bp:
                    break
                take()  # consume op
                right = parse_expr(bp if assoc == 'R' else bp + 1)
                left = {'type': 'binary', 'op': opname, 'left': left, 'right': right}
            return left

        ast = parse_expr(0)
        if peek_type() != 'EOF':
            raise SyntaxError('多余的输入')

        # ============ AST -> CalcStep 流 ============
        steps: List[CalcStep] = []

        def append_step(opname: str) -> int:
            if opname not in OP_FUNCS:
                raise ValueError(f'未定义的函数: {opname}')
            step: CalcStep = {
                'op': OP_FUNCS[opname],
                'args': [],
                'res': 0+0j,
                'res_ptr': -1,  # 稍后回填
            }
            steps.append(step)
            return len(steps) - 1

        # 返回需要把 res_ptr 回填为父节点 index 的步骤索引列表
        def compile_node(node) -> List[int]:
            ntype = node['type']
            if ntype == 'number':
                idx = append_step('push')
                steps[idx]['args'].append(node['value'])
                return [idx]
            if ntype == 'const':
                idx = append_step('push')
                steps[idx]['args'].append(CONST_MAP[node['name']])
                return [idx]
            if ntype == 'unary':
                # 编译子节点 -> 创建当前节点 -> 回填至当前
                pending = compile_node(node['arg'])
                me = append_step(node['op'])
                for p in pending:
                    steps[p]['res_ptr'] = me
                return [me]
            if ntype == 'binary':
                left_p = compile_node(node['left'])
                right_p = compile_node(node['right'])
                me = append_step(node['op'])
                # 按顺序回填，确保参数顺序与出现顺序一致
                for p in left_p + right_p:
                    steps[p]['res_ptr'] = me
                return [me]
            if ntype == 'call':
                # 参数逐个生成 push/子表达式，最后生成函数步骤并回填
                pendings: List[int] = []
                for arg in node['args']:
                    pendings.extend(compile_node(arg))
                me = append_step(node['name'])
                for p in pendings:
                    steps[p]['res_ptr'] = me
                return [me]
            if ntype == 'var':
                me = append_step('var')
                steps[me]['args'].append(node['name'])
                return [me]
            raise ValueError(f'未知节点类型: {ntype}')

        root_pending = compile_node(ast)
        # 顶层输出
        for p in root_pending:
            steps[p]['res_ptr'] = -1

        return steps

# class Force(TypedDict):
#     id:    int # 粒子种类ID(受力) int
#     force: Calcflow  # 力计算流(公式) Calcflow

# class Particle(TypedDict):
#     id:             int  # 粒子种类ID int
#     pid:            int  # 粒子ID int
#     pos:            complex  # 位置 complex
#     vel:            complex  # 速度 complex
#     force:          complex  # 力 complex
#     color:          str  # 颜色 str
#     env_color:      str  # 环境颜色 str
#     lightness:      float  # 亮度 float
#     env_lightness:  float  # 环境亮度 float
#     life_timestamp: float  # 存活时间时间戳 float
#     extra_data:     str  # 额外数据 str

# class id_map():
#     def __init__(self):
#         self.map_base = {
#             "id": 0, # 粒子种类ID(施加力的) int
#             "members": [], # 粒子列表 List[Particle]
#             "force_map": [], # 力计算流列表 List[Force]
#         }
#         self.map = {}
#         self.id: int = 0 # 粒子种类ID(施加力的) int
#         self.members: List[Particle] = [] # 粒子列表 List[Particle]
#         self.force_map: List[Force] = [] # 力计算流列表 List[Force]

#     def add(self, id: int, members: List[Particle], force_map: List[Force]):
#         self.map[id] = {
#             "id": id,
#             "members": members,
#             "force_map": force_map,
#         }

#     def delete(self, id: int):
#         if id in self.map:
#             del self.map[id]

# class particle_map:
#     def __init__(self):
#         self.map_base = {
#             "id": 0,
#             "pid": 0,
#             "pos": 0+0j,
#             "vel": 0+0j,
#             "force": 0+0j,
#             "color": "#FFFFFF", # 颜色 str,当然C++里是char[3]
#             "env_color": "#FFFFFF", # 环境颜色 str,当然C++里是char[3]
#             "lightness": 1.0, # 亮度 float
#             "env_lightness": 1.0, # 环境亮度 float
#             "life_timestamp": 0.0, # 存活时间时间戳 float
#             "extra_data": "", # 额外数据 str char[32]
#         }
#         self.map: List[Particle] = []
#         self.particle_count = 0

#     def add(self, particle: Particle):
#         self.map.append(particle)
#         self.map[-1]["pid"] = self.particle_count
#         self.particle_count += 1

#     def delete(self, pid: int):
#         for i, p in enumerate(self.map):
#             if p["pid"] == pid:
#                 del self.map[i]
#                 break


if __name__ == '__main__':
    # 简单自测：把表达式编译为 CalcStep 流并计算
    def eval_expr(expr: str) -> complex:
        tk = Tokenizer()
        steps = tk.tokenize(expr)
        c = Calc(steps)
        return c.calculate()

    samples = [
        'add(1,2,sin(pi/2))',         # 1 + 2 + 1 = 4
        '1+2*sin(pi/2)',               # 1 + 2*1 = 3
        'pow(2,3) + 4',                # 8 + 4 = 12
        'log(e, e^2)',                 # log_e(e^2) = 2
        'abs(-3) + abs(-4i)',          # 3 + 4 = 7
        'cos(0)+sin(0)+tan(0)',
        '3^2^2',                       # 3^(2^2)=81 右结合
        'sub(10, 1, 2, 3)',            # 10-1-2-3 = 4
        'div(100, 2, 5)',              # 100/2/5 = 10
    ]

    for s in samples:
        try:
            v = eval_expr(s)
            print(f'{s} = {v}')
        except Exception as e:
            print(f'{s} -> 错误: {e}')

    # 变量示例
    print('--- 变量示例 ---')
    tk = Tokenizer()
    steps = tk.tokenize('x^2 + y^2')
    c = Calc(steps)
    print('x=3,y=4 =>', c.calculate(x=3, y=4))  # 25
    print('x=1+2i,y=2 =>', c.calculate(x=1+2j, y=2))
