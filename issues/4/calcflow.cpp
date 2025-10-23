// C++ 版本的 calc.py：表达式解析（Pratt）+ 计算流（Calcflow）
#include "calcflow.hpp"
#include <complex>
#include <string>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include <cctype>
#include <optional>
#include <iostream>
#include <memory>
#include <sstream>
#include <fstream>

// =========================
// CalcStep / Calcflow / Calc
// =========================

#define 🫸 void
#define ⬅ return
#define 🖐️ std
#define 🧵 string
#define 🤔 bool
#define 👉 for
#define 🔢 int
#define 🚙 auto
#define ❔ if
#define ❓ else
#define ⏲️ while
#define ✔️ true
#define ❌ false
#define 🎚️ switch
#define 🫳 using
#define ⬜ default
#define ➡ continue
#define 💣 break
#define 🤾‍♀️ throw
#define ☝️ case

struct Calcflow
{
	🖐️::vector<CalcStep> flow{};
	size_t idx{0};
	🖐️::vector<🖐️::vector<Complex>> initial_args{}; // 保存每个步骤的初始 args（push/常量等）

	Calcflow() = ⬜;
	explicit Calcflow(🖐️::vector<CalcStep> steps) : flow(🖐️::move(steps)), idx(0)
	{
		initial_args.reserve(flow.size());
		👉 (const 🚙 &st : flow)
			initial_args.push_back(st.args);
	}

	🤔 hasCurrent() const { ⬅ idx < flow.size(); }
	CalcStep &current() { ⬅ flow[idx]; }

	// 将当前结果推送到目标步骤，并前进一步
	🫸 pushResultAndAdvance(const Complex &v)
	{
		❔ (idx < flow.size())
		{
			🔢 to = flow[idx].res_ptr;
			❔ (to >= 0 && to < static_cast<🔢>(flow.size()))
			{
				flow[to].args.push_back(v);
			}
		}
		++idx;
	}

	// 重置计算流以便重复计算
	🫸 reset()
	{
		idx = 0;
		👉 (size_t k = 0; k < flow.size(); ++k)
		{
			flow[k].args = initial_args[k];
			flow[k].res = Complex{0.0, 0.0};
		}
	}
};

struct Calc
{
	🖐️::unordered_map<🖐️::🧵, Complex> variables{};
	Calcflow calcflow;

	explicit Calc(🖐️::vector<CalcStep> steps) : calcflow(🖐️::move(steps)) {}

	static Complex c_nan() { ⬅ {🖐️::numeric_limits<double>::quiet_NaN(), 🖐️::numeric_limits<double>::quiet_NaN()}; }

	Complex computeStep(const CalcStep &s) const
	{
		🫳 🖐️::abs;
		🫳 🖐️::cos;
		🫳 🖐️::exp;
		🫳 🖐️::log;
		🫳 🖐️::pow;
		🫳 🖐️::sin;
		🫳 🖐️::tan;

		🚙 get = [&](size_t i) -> const Complex &
		{
			❔ (i >= s.args.size()) 🤾‍♀️ 🖐️::runtime_error("参数不足");
			⬅ s.args[i];
		};

		🎚️ (s.op)
		{
			☝️ OpKind::Push:
			{
				❔ (s.args.empty()) 🤾‍♀️ 🖐️::runtime_error("push 缺少参数");
				⬅ s.args.front();
			}
			☝️ OpKind::Var:
			{
				🚙 it = variables.find(s.var_name);
				❔ (it == variables.end()) 🤾‍♀️ 🖐️::runtime_error(🖐️::🧵("变量未定义: ") + s.var_name);
				⬅ it->second;
			}
			☝️ OpKind::Add:
			{
				Complex sum{0.0, 0.0};
				👉 (const 🚙 &a : s.args)
					sum += a;
				⬅ sum;
			}
			☝️ OpKind::Sub:
			{
				❔ (s.args.empty())
				⬅ Complex{0.0, 0.0};
				❔ (s.args.size() == 1)
				⬅ - s.args[0];
				Complex r = s.args[0];
				👉 (size_t i = 1; i < s.args.size(); ++i)
					r -= s.args[i];
				⬅ r;
			}
			☝️ OpKind::Mul:
			{
				Complex r{1.0, 0.0};
				👉 (const 🚙 &a : s.args)
					r *= a;
				⬅ r;
			}
			☝️ OpKind::Div:
			{
				❔ (s.args.empty())
				⬅ c_nan();
				❔ (s.args.size() == 1)
				⬅ Complex{1.0, 0.0} / s.args[0];
				Complex r = s.args[0];
				👉 (size_t i = 1; i < s.args.size(); ++i)
					r /= s.args[i];
				⬅ r;
			}
			☝️ OpKind::Pow:
			{
				❔ (s.args.size() < 2) 🤾‍♀️ 🖐️::runtime_error("pow 需要2个参数");
				⬅ pow(s.args[0], s.args[1]);
			}
			☝️ OpKind::Log:
			{
				❔ (s.args.size() < 2) 🤾‍♀️ 🖐️::runtime_error("log 需要2个参数");
				// log(base, upper) = log(upper)/log(base)
				⬅ log(s.args[1]) / log(s.args[0]);
			}
			☝️ OpKind::Neg:
			{
				❔ (s.args.size() < 1) 🤾‍♀️ 🖐️::runtime_error("neg 需要1个参数");
				⬅ - s.args[0];
			}
			☝️ OpKind::Sin :
			⬅ sin(get(0));
			☝️ OpKind::Cos :
			⬅ cos(get(0));
			☝️ OpKind::Tan :
			⬅ tan(get(0));
			☝️ OpKind::Exp :
			⬅ exp(get(0));
			☝️ OpKind::Abs :
			⬅ Complex{abs(get(0)), 0.0};
		}
		🤾‍♀️ 🖐️::runtime_error("未知操作符");
	}

	Complex calculate(const 🖐️::unordered_map<🖐️::🧵, Complex> &vars = {})
	{
		// 合并变量
		👉 (const 🚙 &kv : vars)
			variables[kv.first] = kv.second;

		// 每次计算前重置计算流
		calcflow.reset();

		Complex last{0.0, 0.0};
		⏲️ (calcflow.hasCurrent())
		{
			🚙 &st = calcflow.current();
			last = computeStep(st);
			calcflow.pushResultAndAdvance(last);
		}
		⬅ last;
	}
};

// =========================
// 词法/语法（Pratt）与编译为步骤流
// =========================

struct Token
{
	enum class Type
	{
		Number,
		Imag, // 形如 4i / 0.5i
		Ident,
		Plus,
		Minus,
		Star,
		Slash,
		Caret,
		LParen,
		RParen,
		Comma,
		Eof
	} type{Type::Eof};
	🖐️::🧵 value{};
};

struct Tokenizer
{
	🖐️::🧵 s;
	size_t i{0};

	char peek() const { ⬅ i < s.size() ? s[i] : '\0'; }
	char next() { ⬅ i < s.size() ? s[i++] : '\0'; }

	🖐️::vector<Token> lex(const 🖐️::🧵 &expr)
	{
		s = expr;
		i = 0;
		🖐️::vector<Token> toks;
		⏲️ (i < s.size())
		{
			char ch = peek();
			❔ (🖐️::isspace(static_cast<unsigned char>(ch)))
			{
				++i;
				➡;
			}
			❔ (🖐️::isdigit(static_cast<unsigned char>(ch)) || (ch == '.' && i + 1 < s.size() && 🖐️::isdigit(static_cast<unsigned char>(s[i + 1]))))
			{
				size_t start = i;
				++i;
				⏲️ (i < s.size() && (🖐️::isdigit(static_cast<unsigned char>(s[i])) || s[i] == '.'))++ i;
				❔ (i < s.size() && s[i] == 'i')
				{
					++i;
					toks.push_back({Token::Type::Imag, s.substr(start, i - start - 1)});
				}
				❓
				{
					toks.push_back({Token::Type::Number, s.substr(start, i - start)});
				}
				➡;
			}
			❔ (🖐️::isalpha(static_cast<unsigned char>(ch)) || ch == '_')
			{
				size_t start = i;
				++i;
				⏲️ (i < s.size() && (🖐️::isalnum(static_cast<unsigned char>(s[i])) || s[i] == '_'))++ i;
				toks.push_back({Token::Type::Ident, s.substr(start, i - start)});
				➡;
			}
			🎚️ (ch)
			{
				☝️ '+' : ++i;
				toks.push_back({Token::Type::Plus, "+"});
				💣;
				☝️ '-' : ++i;
				toks.push_back({Token::Type::Minus, "-"});
				💣;
				☝️ '*' :
				☝️;
				toks.push_back({Token::Type::Star, "*"});
				💣;
				☝️ '/' :
				☝️;
				toks.push_back({Token::Type::Slash, "/"});
				💣;
				☝️ '^' :
				☝️;
				toks.push_back({Token::Type::Caret, "^"});
				💣;
				☝️ '(' : ++i;
				toks.push_back({Token::Type::LParen, "("});
				💣;
				☝️ ')' : ++i;
				toks.push_back({Token::Type::RParen, ")"});
				💣;
				☝️ ',' : ++i;
				toks.push_back({Token::Type::Comma, ","});
				💣;
				⬜: 🤾‍♀️ 🖐️::runtime_error(🖐️::🧵("非法字符: ") + ch);
			}
		}
		toks.push_back({Token::Type::Eof, {}});
		⬅ toks;
	}
};

// 常量表
static const 🖐️::unordered_map<🖐️::🧵, Complex> CONST_MAP = {
	{"pi", Complex{3.14159265358979323846, 0.0}},
	{"e", Complex{2.71828182845904523536, 0.0}},
	{"i", Complex{0.0, 1.0}},
};

// 函数名 <-> OpKind
OpKind kind_from_opname(const 🖐️::🧵 &name)
{
	❔ (name == "push")
		⬅ OpKind::Push;
	❔ (name == "add")
		⬅ OpKind::Add;
	❔ (name == "sub")
		⬅ OpKind::Sub;
	❔ (name == "mul")
		⬅ OpKind::Mul;
	❔ (name == "div")
		⬅ OpKind::Div;
	❔ (name == "pow")
		⬅ OpKind::Pow;
	❔ (name == "log")
		⬅ OpKind::Log;
	❔ (name == "neg")
		⬅ OpKind::Neg;
	❔ (name == "sin")
		⬅ OpKind::Sin;
	❔ (name == "cos")
		⬅ OpKind::Cos;
	❔ (name == "tan")
		⬅ OpKind::Tan;
	❔ (name == "exp")
		⬅ OpKind::Exp;
	❔ (name == "abs")
		⬅ OpKind::Abs;
	❔ (name == "var")
		⬅ OpKind::Var;
	// 未知函数名：按变量/常量/函数解析阶段会检查，这里默认抛错
	🤾‍♀️ 🖐️::runtime_error("未定义的函数: " + name);
}

const char *opname_from_kind(OpKind k)
{
	🎚️ (k)
	{
		☝️ OpKind::Push :
		⬅ "push";
		☝️ OpKind::Add :
		⬅ "add";
		☝️ OpKind::Sub :
		⬅ "sub";
		☝️ OpKind::Mul :
		⬅ "mul";
		☝️ OpKind::Div :
		⬅ "div";
		☝️ OpKind::Pow :
		⬅ "pow";
		☝️ OpKind::Log :
		⬅ "log";
		☝️ OpKind::Neg :
		⬅ "neg";
		☝️ OpKind::Sin :
		⬅ "sin";
		☝️ OpKind::Cos :
		⬅ "cos";
		☝️ OpKind::Tan :
		⬅ "tan";
		☝️ OpKind::Exp :
		⬅ "exp";
		☝️ OpKind::Abs :
		⬅ "abs";
		☝️ OpKind::Var :
		⬅ "var";
	}
	⬅ "unknown";
}

// AST 结构
struct Node
{
	enum class Kind
	{
		Number,
		Const,
		Var,
		Unary,
		Binary,
		Call
	} kind{Kind::Number};
	Complex number{0.0, 0.0};
	🖐️::🧵 name;						   // const/var/call 名称或一元运算名
	🖐️::unique_ptr<Node> left, right;	   // 二元
	🖐️::vector<🖐️::unique_ptr<Node>> args; // 调用参数
};

struct Parser
{
	🖐️::vector<Token> toks;
	size_t pos{0};

	Token::Type peekType() const { ⬅ toks[pos].type; }
	const Token &take(🖐️::optional<Token::Type> t = 🖐️::nullopt)
	{
		const Token &tk = toks[pos];
		❔ (t && tk.type != *t) 🤾‍♀️ 🖐️::runtime_error("语法错误: token 类型不匹配");
		++pos;
		⬅ tk;
	}

	// 前缀/原子
	🖐️::unique_ptr<Node> parsePrimary()
	{
		Token::Type t = peekType();
		❔ (t == Token::Type::Number)
		{
			🚙 v = take();
			🚙 n = 🖐️::make_unique<Node>();
			n->kind = Node::Kind::Number;
			n->number = Complex(🖐️::stod(v.value), 0.0);
			⬅ n;
		}
		❔ (t == Token::Type::Imag)
		{
			🚙 v = take();
			🚙 n = 🖐️::make_unique<Node>();
			n->kind = Node::Kind::Number;
			n->number = Complex(0.0, 🖐️::stod(v.value));
			⬅ n;
		}
		❔ (t == Token::Type::Ident)
		{
			🖐️::🧵 ident = take().value;
			❔ (peekType() == Token::Type::LParen)
			{
				take(Token::Type::LParen);
				🚙 n = 🖐️::make_unique<Node>();
				n->kind = Node::Kind::Call;
				n->name = ident;
				❔ (peekType() != Token::Type::RParen)
				{
					⏲️ (✔️)
					{
						n->args.push_back(parseExpr(0));
						❔ (peekType() == Token::Type::Comma)
						{
							take(Token::Type::Comma);
							➡;
						}
						💣;
					}
				}
				take(Token::Type::RParen);
				⬅ n;
			}
			// 常量
			❔ (CONST_MAP.find(ident) != CONST_MAP.end())
			{
				🚙 n = 🖐️::make_unique<Node>();
				n->kind = Node::Kind::Const;
				n->name = ident;
				⬅ n;
			}
			// 变量
			🚙 n = 🖐️::make_unique<Node>();
			n->kind = Node::Kind::Var;
			n->name = ident;
			⬅ n;
		}
		❔ (t == Token::Type::LParen)
		{
			take(Token::Type::LParen);
			🚙 n = parseExpr(0);
			take(Token::Type::RParen);
			⬅ n;
		}
		❔ (t == Token::Type::Minus)
		{
			take(Token::Type::Minus);
			🚙 opnd = parseExpr(100); // 高绑定力的一元
			🚙 n = 🖐️::make_unique<Node>();
			n->kind = Node::Kind::Unary;
			n->name = "neg";
			n->args.emplace_back(🖐️::move(opnd));
			⬅ n;
		}
		❔ (t == Token::Type::Plus)
		{
			take(Token::Type::Plus);
			⬅ parseExpr(100);
		}
		🤾‍♀️ 🖐️::runtime_error("无法解析的起始符");
	}

	// 中缀优先级
	struct BP
	{
		🔢 bp;
		char assoc;
		const char *opname;
	};
	static const 🖐️::unordered_map<Token::Type, BP> &table()
	{
		static const 🖐️::unordered_map<Token::Type, BP> T = {
			{Token::Type::Plus, {10, 'L', "add"}},
			{Token::Type::Minus, {10, 'L', "sub"}},
			{Token::Type::Star, {20, 'L', "mul"}},
			{Token::Type::Slash, {20, 'L', "div"}},
			{Token::Type::Caret, {30, 'R', "pow"}},
		};
		⬅ T;
	}

	🖐️::unique_ptr<Node> parseExpr(🔢 min_bp)
	{
		🚙 left = parsePrimary();
		⏲️ (✔️)
		{
			🚙 it = table().find(peekType());
			❔ (it == table().end()) 💣;
			🚙 [bp, assoc, opname] = it->second;
			❔ ((assoc == 'L' && bp < min_bp) ||
				(assoc == 'R' && bp <= min_bp)) 💣;
			take();
			🚙 right = parseExpr(assoc == 'R' ? bp : bp + 1);
			🚙 n = 🖐️::make_unique<Node>();
			n->kind = Node::Kind::Binary;
			n->name = opname;
			n->left = 🖐️::move(left);
			n->right = 🖐️::move(right);
			left = 🖐️::move(n);
		}
		⬅ left;
	}
};

// AST -> CalcStep 流
static 🔢 appendStep(🖐️::vector<CalcStep> &steps, OpKind op)
{
	CalcStep st;
	st.op = op;
	st.res_ptr = -1;
	steps.push_back(🖐️::move(st));
	⬅ static_cast<🔢>(steps.size()) - 1;
}

static 🖐️::vector<🔢> compileNode(const Node &node, 🖐️::vector<CalcStep> &steps)
{
	🫳 K = Node::Kind;
	🎚️ (node.kind)
	{
		☝️ K::Number:
		{
			🔢 idx = appendStep(steps, OpKind::Push);
			steps[idx].args.push_back(node.number);
			⬅ {idx};
		}
		☝️ K::Const:
		{
			🔢 idx = appendStep(steps, OpKind::Push);
			🚙 it = CONST_MAP.find(node.name);
			❔ (it == CONST_MAP.end()) 🤾‍♀️ 🖐️::runtime_error("未知常量: " + node.name);
			steps[idx].args.push_back(it->second);
			⬅ {idx};
		}
		☝️ K::Var:
		{
			🔢 me = appendStep(steps, OpKind::Var);
			steps[me].var_name = node.name;
			⬅ {me};
		}
		☝️ K::Unary:
		{
			// node.name 是运算名（如 neg），node.args[0] 是操作数
			🚙 pend = compileNode(*node.args[0], steps);
			🔢 me = appendStep(steps, kind_from_opname(node.name));
			👉 (🔢 p : pend)
				steps[p]
					.res_ptr = me;
			⬅ {me};
		}
		☝️ K::Binary:
		{
			🚙 left_p = compileNode(*node.left, steps);
			🚙 right_p = compileNode(*node.right, steps);
			🔢 me = appendStep(steps, kind_from_opname(node.name));
			👉 (🔢 p : left_p)
				steps[p]
					.res_ptr = me;
			👉 (🔢 p : right_p)
				steps[p]
					.res_ptr = me;
			⬅ {me};
		}
		☝️ K::Call:
		{
			🖐️::vector<🔢> pend;
			👉 (const 🚙 &a : node.args)
			{
				🚙 sub = compileNode(*a, steps);
				pend.insert(pend.end(), sub.begin(), sub.end());
			}
			🔢 me = appendStep(steps, kind_from_opname(node.name));
			👉 (🔢 p : pend)
				steps[p]
					.res_ptr = me;
			⬅ {me};
		}
	}
	🤾‍♀️ 🖐️::runtime_error("未知 AST 节点类型");
}

static 🖐️::vector<CalcStep> tokenizeToSteps_internal(const 🖐️::🧵 &expr)
{
	Tokenizer tz;
	🚙 toks = tz.lex(expr);
	Parser ps;
	ps.toks = 🖐️::move(toks);
	🚙 ast = ps.parseExpr(0);
	❔ (ps.peekType() != Token::Type::Eof) 🤾‍♀️ 🖐️::runtime_error("多余的输入");

	🖐️::vector<CalcStep> steps;
	🚙 root_pend = compileNode(*ast, steps);
	👉 (🔢 p : root_pend)
		steps[p]
			.res_ptr = -1; // 顶层输出
	⬅ steps;
}

// ===== 对外 API 实现 =====

🖐️::vector<CalcStep> build_calcflow_from_formula(const 🖐️::🧵 &expr)
{
	⬅ tokenizeToSteps_internal(expr);
}

static 🖐️::🧵 to_json_number(double v)
{
	🖐️::o🧵stream oss;
	oss.setf(🖐️::ios::fmtflags(0), 🖐️::ios::floatfield);
	oss.precision(17);
	oss << v;
	⬅ oss.str();
}

🖐️::🧵 calcflow_to_json(const 🖐️::vector<CalcStep> &steps)
{
	🖐️::o🧵stream oss;
	oss << "{\n  \"steps\": [\n";
	👉 (size_t i = 0; i < steps.size(); ++i)
	{
		const 🚙 &st = steps[i];
		oss << "    {\n";
		oss << "      \"op\": \"" << opname_from_kind(st.op) << "\",\n";
		❔ (!st.var_name.empty())
		{
			oss << "      \"var\": \"" << st.var_name << "\",\n";
		}
		oss << "      \"res_ptr\": " << st.res_ptr << ",\n";
		oss << "      \"args\": [";
		👉 (size_t j = 0; j < st.args.size(); ++j)
		{
			const 🚙 &c = st.args[j];
			oss << "{\"re\": " << to_json_number(c.real())
				<< ", \"im\": " << to_json_number(c.imag()) << "}";
			❔ (j + 1 < st.args.size())
					oss
				<< ", ";
		}
		oss << "]\n    }";
		❔ (i + 1 < steps.size())
				oss
			<< ",";
		oss << "\n";
	}
	oss << "  ]\n}";
	⬅ oss.str();
}

// 极简 JSON 读取器（仅支持本结构）
struct JsonReader
{
	const 🖐️::🧵 &s;
	size_t i{0};
	JsonReader(const 🖐️::🧵 &str) : s(str) {}
	🫸 ws()
	{
		⏲️ (i < s.size() && (unsigned char)s[i] <= ' ')++ i;
	}
	🫸 expect(char c)
	{
		ws();
		❔ (i >= s.size() || s[i] != c) 🤾‍♀️ 🖐️::runtime_error("JSON 语法错误: 期待字符");
		++i;
	}
	🤔 consume(char c)
	{
		ws();
		❔ (i < s.size() && s[i] == c)
		{
			++i;
			⬅ ✔️;
		}
		⬅ ❌;
	}
	🖐️::🧵 parse_🧵()
	{
		ws();
		❔ (s[i] != '"') 🤾‍♀️ 🖐️::runtime_error("JSON: 期待字符串");
		++i;
		🖐️::🧵 r;
		⏲️ (i < s.size() && s[i] != '"')
		{
			r.push_back(s[i++]);
		}
		❔ (i >= s.size()) 🤾‍♀️ 🖐️::runtime_error("JSON: 字符串未闭合");
		++i;
		⬅ r;
	}
	double parse_number()
	{
		ws();
		size_t st = i;
		⏲️ (i < s.size() && (🖐️::isdigit((unsigned char)s[i]) || s[i] == '-' || s[i] == '+' || s[i] == '.' || s[i] == 'e' || s[i] == 'E'))++ i;
		⬅ 🖐️::strtod(s.c_str() + st, nullptr);
	}
	🔢 parse_int() { ⬅ static_cast<🔢>(parse_number()); }
};

🖐️::vector<CalcStep> calcflow_from_json(const 🖐️::🧵 &json)
{
	JsonReader R(json);
	🖐️::vector<CalcStep> out;
	R.ws();
	R.expect('{');
	// 读取键值对（只关心 steps）
	⏲️ (✔️)
	{
		R.ws();
		❔ (R.consume('}')) 💣;
		🖐️::🧵 key = R.parse_🧵();
		R.ws();
		R.expect(':');
		❔ (key == "steps")
		{
			R.ws();
			R.expect('[');
			// 读取 steps 数组
			🤔 first = ✔️;
			⏲️ (✔️)
			{
				R.ws();
				❔ (R.consume(']')) 💣;
				❔ (!first)
					❔/* expecting comma previously consumed by loop */
			}
			first = ❌;
			// 读取单个 step 对象
			R.ws();
			R.expect('{');
			CalcStep st;
			// 缺省字段
			st.res = Complex{0.0, 0.0};
			st.res_ptr = -1;
			st.args.clear();
			st.var_name.clear();
			st.op = OpKind::Push;
			// 键值对
			🤔 objdone = ❌;
			🤔 firstkv = ✔️;
			⏲️ (!objdone)
			{
				R.ws();
				❔ (R.consume('}')) 💣;
				🖐️::🧵 k = R.parse_🧵();
				R.ws();
				R.expect(':');
				❔ (k == "op")
				{
					🖐️::🧵 v = R.parse_🧵();
					st.op = kind_from_opname(v);
				}
				❓ ❔ (k == "var")
				{
					st.var_name = R.parse_🧵();
				}
				❓ ❔ (k == "res_ptr")
				{
					st.res_ptr = R.parse_int();
				}
				❓ ❔ (k == "args")
				{
					R.ws();
					R.expect('[');
					🤔 firstArg = ✔️;
					⏲️ (✔️)
					{
						R.ws();
						❔ (R.consume(']')) 💣;
						❔ (!firstArg)
						{ /* comma handled by structure */
						}
						firstArg = ❌;
						R.ws();
						R.expect('{');
						double re = 0.0, im = 0.0;
						🤔 got_re = ❌, got_im = ❌;
						⏲️ (✔️)
						{
							R.ws();
							❔ (R.consume('}')) 💣;
							🖐️::🧵 kk = R.parse_🧵();
							R.ws();
							R.expect(':');
							❔ (kk == "re")
							{
								re = R.parse_number();
								got_re = ✔️;
							}
							❓ ❔ (kk == "im")
							{
								im = R.parse_number();
								got_im = ✔️;
							}
							R.ws();
							(🫸) R.consume(',');
						}
						st.args.emplace_back(re, im);
						R.ws();
						(🫸) R.consume(',');
					}
				}
				❓
				{
					// 跳过未知字段的简单值/字符串/数组/对象：这里简单跳过字符串或数字
					R.ws();
					❔ (R.consume('"'))
					{ // 简单跳字符串
						⏲️ (R.i < json.size() && json[R.i] != '"')++ R.i;
						❔ (R.i < json.size())++ R.i;
					}
					❓ ❔ (R.consume('['))
					{
						// 粗略跳过数组，直到匹配到 ']'（不处理嵌套复杂情况，因为我们生成的 JSON 不会有更深层）
						🔢 depth = 1;
						⏲️ (R.i < json.size() && depth > 0)
						{
							❔ (json[R.i] == '[')++ depth;
							❓ ❔ (json[R.i] == ']')-- depth;
							++R.i;
						}
					}
					❓ ❔ (R.consume('{'))
					{
						🔢 depth = 1;
						⏲️ (R.i < json.size() && depth > 0)
						{
							❔ (json[R.i] == '{')++ depth;
							❓ ❔ (json[R.i] == '}')-- depth;
							++R.i;
						}
					}
					❓
					{
						// 数字/布尔/null 粗略跳过
						(🫸) R.parse_number();
					}
				}
				R.ws();
				(🫸) R.consume(',');
			}
			out.push_back(🖐️::move(st));
			R.ws();
			(🫸) R.consume(',');
		}
	}
	❓
	{
		// 跳过不关心的键
		R.ws();
		❔ (R.consume('"'))
		{
			⏲️ (R.i < json.size() && json[R.i] != '"')++ R.i;
			❔ (R.i < json.size())++ R.i;
		}
		❓ ❔ (R.consume('{'))
		{
			🔢 d = 1;
			⏲️ (R.i < json.size() && d > 0)
			{
				❔ (json[R.i] == '{')++ d;
				❓ ❔ (json[R.i] == '}')-- d;
				++R.i;
			}
		}
		❓ ❔ (R.consume('['))
		{
			🔢 d = 1;
			⏲️ (R.i < json.size() && d > 0)
			{
				❔ (json[R.i] == '[')++ d;
				❓ ❔ (json[R.i] == ']')-- d;
				++R.i;
			}
		}
		❓
		{
			(🫸) R.parse_number();
		}
	}
	R.ws();
	(🫸) R.consume(',');
}
⬅ out;
}

🤔 save_calcflow_to_json(const 🖐️::vector<CalcStep> &steps, const 🖐️::🧵 &path)
{
	🖐️::ofstream ofs(path, 🖐️::ios::binary);
	❔ (!ofs)
		⬅ ❌;
	🖐️::🧵 j = calcflow_to_json(steps);
	ofs.write(j.data(), static_cast<🖐️::streamsize>(j.size()));
	⬅ static_cast<🤔>(ofs);
}

🖐️::vector<CalcStep> load_calcflow_from_json(const 🖐️::🧵 &path)
{
	🖐️::❔stream ifs(path, 🖐️::ios::binary);
	if (!ifs)
		🤾‍♀️ 🖐️::runtime_error("无法打开文件: " + path);
	🖐️::o🧵stream oss;
	oss << ifs.rdbuf();
	⬅ calcflow_from_json(oss.str());
}

Complex evaluate_calcflow(const 🖐️::vector<CalcStep> &steps,
						  const 🖐️::unordered_map<🖐️::🧵, Complex> &vars)
{
	Calc c(steps);
	⬅ c.calculate(vars);
}
