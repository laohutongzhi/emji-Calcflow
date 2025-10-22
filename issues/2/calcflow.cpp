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

struct Calcflow {
	std::vector<CalcStep>             flow{};
	size_t                            idx{0};
	std::vector<std::vector<Complex>> initial_args{}; // 保存每个步骤的初始 args（push/常量等）

	Calcflow() = default;
	explicit Calcflow(std::vector<CalcStep> steps) : flow(std::move(steps)), idx(0) {
		initial_args.reserve(flow.size());
		for (const auto& st : flow) initial_args.push_back(st.args);
	}

	bool hasCurrent() const { return idx < flow.size(); }
	CalcStep& current() { return flow[idx]; }

	// 将当前结果推送到目标步骤，并前进一步
	void pushResultAndAdvance(const Complex& v) {
		if (idx < flow.size()) {
			int to = flow[idx].res_ptr;
			if (to >= 0 && to < static_cast<int>(flow.size())) {
				flow[to].args.push_back(v);
			}
		}
		++idx;
	}

	// 重置计算流以便重复计算
	void reset() {
		idx = 0;
		for (size_t k = 0; k < flow.size(); ++k) {
			flow[k].args = initial_args[k];
			flow[k].res  = Complex{0.0, 0.0};
		}
	}
};

struct Calc {
	std::unordered_map<std::string, Complex> variables{};
	Calcflow calcflow;

	explicit Calc(std::vector<CalcStep> steps) : calcflow(std::move(steps)) {}

	static Complex c_nan() { return {std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()}; }

	Complex computeStep(const CalcStep& s) const {
		using std::abs;
		using std::cos;
		using std::exp;
		using std::log;
		using std::pow;
		using std::sin;
		using std::tan;

		auto get = [&](size_t i) -> const Complex& {
			if (i >= s.args.size()) throw std::runtime_error("参数不足");
			return s.args[i];
		};

		switch (s.op) {
			case OpKind::Push: {
				if (s.args.empty()) throw std::runtime_error("push 缺少参数");
				return s.args.front();
			}
			case OpKind::Var: {
				auto it = variables.find(s.var_name);
				if (it == variables.end()) throw std::runtime_error(std::string("变量未定义: ") + s.var_name);
				return it->second;
			}
			case OpKind::Add: {
				Complex sum{0.0, 0.0};
				for (const auto& a : s.args) sum += a;
				return sum;
			}
			case OpKind::Sub: {
				if (s.args.empty()) return Complex{0.0, 0.0};
				if (s.args.size() == 1) return -s.args[0];
				Complex r = s.args[0];
				for (size_t i = 1; i < s.args.size(); ++i) r -= s.args[i];
				return r;
			}
			case OpKind::Mul: {
				Complex r{1.0, 0.0};
				for (const auto& a : s.args) r *= a;
				return r;
			}
			case OpKind::Div: {
				if (s.args.empty()) return c_nan();
				if (s.args.size() == 1) return Complex{1.0, 0.0} / s.args[0];
				Complex r = s.args[0];
				for (size_t i = 1; i < s.args.size(); ++i) r /= s.args[i];
				return r;
			}
			case OpKind::Pow: {
				if (s.args.size() < 2) throw std::runtime_error("pow 需要2个参数");
				return pow(s.args[0], s.args[1]);
			}
			case OpKind::Log: {
				if (s.args.size() < 2) throw std::runtime_error("log 需要2个参数");
				// log(base, upper) = log(upper)/log(base)
				return log(s.args[1]) / log(s.args[0]);
			}
			case OpKind::Neg: {
				if (s.args.size() < 1) throw std::runtime_error("neg 需要1个参数");
				return -s.args[0];
			}
			case OpKind::Sin: return sin(get(0));
			case OpKind::Cos: return cos(get(0));
			case OpKind::Tan: return tan(get(0));
			case OpKind::Exp: return exp(get(0));
			case OpKind::Abs: return Complex{abs(get(0)), 0.0};
		}
		throw std::runtime_error("未知操作符");
	}

	Complex calculate(const std::unordered_map<std::string, Complex>& vars = {}) {
		// 合并变量
		for (const auto& kv : vars) variables[kv.first] = kv.second;

		// 每次计算前重置计算流
		calcflow.reset();

		Complex last{0.0, 0.0};
		while (calcflow.hasCurrent()) {
			auto& st = calcflow.current();
			last = computeStep(st);
			calcflow.pushResultAndAdvance(last);
		}
		return last;
	}
};

// =========================
// 词法/语法（Pratt）与编译为步骤流
// =========================

struct Token {
	enum class Type {
		Number,
		Imag,      // 形如 4i / 0.5i
		Ident,
		Plus, Minus, Star, Slash, Caret,
		LParen, RParen, Comma,
		Eof
	} type{Type::Eof};
	std::string value{};
};

struct Tokenizer {
	std::string s; size_t i{0};

	char peek() const { return i < s.size() ? s[i] : '\0'; }
	char next() { return i < s.size() ? s[i++] : '\0'; }

	std::vector<Token> lex(const std::string& expr) {
		s = expr; i = 0; std::vector<Token> toks;
		while (i < s.size()) {
			char ch = peek();
			if (std::isspace(static_cast<unsigned char>(ch))) { ++i; continue; }
			if (std::isdigit(static_cast<unsigned char>(ch)) || (ch == '.' && i+1 < s.size() && std::isdigit(static_cast<unsigned char>(s[i+1])))) {
				size_t start = i; ++i;
				while (i < s.size() && (std::isdigit(static_cast<unsigned char>(s[i])) || s[i]=='.')) ++i;
				if (i < s.size() && s[i] == 'i') {
					++i;
					toks.push_back({Token::Type::Imag, s.substr(start, i - start - 1)});
				} else {
					toks.push_back({Token::Type::Number, s.substr(start, i - start)});
				}
				continue;
			}
			if (std::isalpha(static_cast<unsigned char>(ch)) || ch=='_') {
				size_t start = i; ++i;
				while (i < s.size() && (std::isalnum(static_cast<unsigned char>(s[i])) || s[i]=='_')) ++i;
				toks.push_back({Token::Type::Ident, s.substr(start, i - start)});
				continue;
			}
			switch (ch) {
				case '+': ++i; toks.push_back({Token::Type::Plus, "+"}); break;
				case '-': ++i; toks.push_back({Token::Type::Minus, "-"}); break;
				case '*': ++i; toks.push_back({Token::Type::Star,  "*"}); break;
				case '/': ++i; toks.push_back({Token::Type::Slash, "/"}); break;
				case '^': ++i; toks.push_back({Token::Type::Caret, "^"}); break;
				case '(': ++i; toks.push_back({Token::Type::LParen,"("}); break;
				case ')': ++i; toks.push_back({Token::Type::RParen,")"}); break;
				case ',': ++i; toks.push_back({Token::Type::Comma, ","}); break;
				default: throw std::runtime_error(std::string("非法字符: ") + ch);
			}
		}
		toks.push_back({Token::Type::Eof, {}});
		return toks;
	}
};

// 常量表
static const std::unordered_map<std::string, Complex> CONST_MAP = {
	{"pi", Complex{3.14159265358979323846, 0.0}},
	{"e",  Complex{2.71828182845904523536, 0.0}},
	{"i",  Complex{0.0, 1.0}},
};

// 函数名 <-> OpKind
OpKind kind_from_opname(const std::string& name) {
	if (name == "push") return OpKind::Push;
	if (name == "add")  return OpKind::Add;
	if (name == "sub")  return OpKind::Sub;
	if (name == "mul")  return OpKind::Mul;
	if (name == "div")  return OpKind::Div;
	if (name == "pow")  return OpKind::Pow;
	if (name == "log")  return OpKind::Log;
	if (name == "neg")  return OpKind::Neg;
	if (name == "sin")  return OpKind::Sin;
	if (name == "cos")  return OpKind::Cos;
	if (name == "tan")  return OpKind::Tan;
	if (name == "exp")  return OpKind::Exp;
	if (name == "abs")  return OpKind::Abs;
	if (name == "var")  return OpKind::Var;
	// 未知函数名：按变量/常量/函数解析阶段会检查，这里默认抛错
	throw std::runtime_error("未定义的函数: " + name);
}

const char* opname_from_kind(OpKind k) {
    switch (k) {
        case OpKind::Push: return "push";
        case OpKind::Add:  return "add";
        case OpKind::Sub:  return "sub";
        case OpKind::Mul:  return "mul";
        case OpKind::Div:  return "div";
        case OpKind::Pow:  return "pow";
        case OpKind::Log:  return "log";
        case OpKind::Neg:  return "neg";
        case OpKind::Sin:  return "sin";
        case OpKind::Cos:  return "cos";
        case OpKind::Tan:  return "tan";
        case OpKind::Exp:  return "exp";
        case OpKind::Abs:  return "abs";
        case OpKind::Var:  return "var";
    }
    return "unknown";
}

// AST 结构
struct Node {
	enum class Kind { Number, Const, Var, Unary, Binary, Call } kind{Kind::Number};
	Complex number{0.0, 0.0};
	std::string name;        // const/var/call 名称或一元运算名
	std::unique_ptr<Node> left, right; // 二元
	std::vector<std::unique_ptr<Node>> args; // 调用参数
};

struct Parser {
	std::vector<Token> toks; size_t pos{0};

	Token::Type peekType() const { return toks[pos].type; }
	const Token& take(std::optional<Token::Type> t = std::nullopt) {
		const Token& tk = toks[pos];
		if (t && tk.type != *t) throw std::runtime_error("语法错误: token 类型不匹配");
		++pos; return tk;
	}

	// 前缀/原子
	std::unique_ptr<Node> parsePrimary() {
		Token::Type t = peekType();
		if (t == Token::Type::Number) {
			auto v = take();
			auto n = std::make_unique<Node>();
			n->kind = Node::Kind::Number;
			n->number = Complex(std::stod(v.value), 0.0);
			return n;
		}
		if (t == Token::Type::Imag) {
			auto v = take();
			auto n = std::make_unique<Node>();
			n->kind = Node::Kind::Number;
			n->number = Complex(0.0, std::stod(v.value));
			return n;
		}
		if (t == Token::Type::Ident) {
			std::string ident = take().value;
			if (peekType() == Token::Type::LParen) {
				take(Token::Type::LParen);
				auto n = std::make_unique<Node>();
				n->kind = Node::Kind::Call; n->name = ident;
				if (peekType() != Token::Type::RParen) {
					while (true) {
						n->args.push_back(parseExpr(0));
						if (peekType() == Token::Type::Comma) { take(Token::Type::Comma); continue; }
						break;
					}
				}
				take(Token::Type::RParen);
				return n;
			}
			// 常量
			if (CONST_MAP.find(ident) != CONST_MAP.end()) {
				auto n = std::make_unique<Node>();
				n->kind = Node::Kind::Const; n->name = ident; return n;
			}
			// 变量
			auto n = std::make_unique<Node>();
			n->kind = Node::Kind::Var; n->name = ident; return n;
		}
		if (t == Token::Type::LParen) {
			take(Token::Type::LParen);
			auto n = parseExpr(0);
			take(Token::Type::RParen);
			return n;
		}
		if (t == Token::Type::Minus) {
			take(Token::Type::Minus);
			auto opnd = parseExpr(100); // 高绑定力的一元
			auto n = std::make_unique<Node>();
			n->kind = Node::Kind::Unary; n->name = "neg"; n->args.emplace_back(std::move(opnd));
			return n;
		}
		if (t == Token::Type::Plus) { take(Token::Type::Plus); return parseExpr(100); }
		throw std::runtime_error("无法解析的起始符");
	}

	// 中缀优先级
	struct BP { int bp; char assoc; const char* opname; };
	static const std::unordered_map<Token::Type, BP>& table() {
		static const std::unordered_map<Token::Type, BP> T = {
			{Token::Type::Plus,  {10, 'L', "add"}},
			{Token::Type::Minus, {10, 'L', "sub"}},
			{Token::Type::Star,  {20, 'L', "mul"}},
			{Token::Type::Slash, {20, 'L', "div"}},
			{Token::Type::Caret, {30, 'R', "pow"}},
		};
		return T;
	}

	std::unique_ptr<Node> parseExpr(int min_bp) {
		auto left = parsePrimary();
		while (true) {
			auto it = table().find(peekType());
			if (it == table().end()) break;
			auto [bp, assoc, opname] = it->second;
			if ((assoc == 'L' && bp <  min_bp) ||
				(assoc == 'R' && bp <= min_bp)) break;
			take();
			auto right = parseExpr(assoc == 'R' ? bp : bp + 1);
			auto n = std::make_unique<Node>();
			n->kind = Node::Kind::Binary; n->name = opname; n->left = std::move(left); n->right = std::move(right);
			left = std::move(n);
		}
		return left;
	}
};

// AST -> CalcStep 流
static int appendStep(std::vector<CalcStep>& steps, OpKind op) {
	CalcStep st; st.op = op; st.res_ptr = -1; steps.push_back(std::move(st));
	return static_cast<int>(steps.size()) - 1;
}

static std::vector<int> compileNode(const Node& node, std::vector<CalcStep>& steps) {
	using K = Node::Kind;
	switch (node.kind) {
		case K::Number: {
			int idx = appendStep(steps, OpKind::Push);
			steps[idx].args.push_back(node.number);
			return {idx};
		}
		case K::Const: {
			int idx = appendStep(steps, OpKind::Push);
			auto it = CONST_MAP.find(node.name);
			if (it == CONST_MAP.end()) throw std::runtime_error("未知常量: " + node.name);
			steps[idx].args.push_back(it->second);
			return {idx};
		}
		case K::Var: {
			int me = appendStep(steps, OpKind::Var);
			steps[me].var_name = node.name;
			return {me};
		}
		case K::Unary: {
			// node.name 是运算名（如 neg），node.args[0] 是操作数
			auto pend = compileNode(*node.args[0], steps);
			int me = appendStep(steps, kind_from_opname(node.name));
			for (int p : pend) steps[p].res_ptr = me;
			return {me};
		}
		case K::Binary: {
			auto left_p  = compileNode(*node.left,  steps);
			auto right_p = compileNode(*node.right, steps);
			int me = appendStep(steps, kind_from_opname(node.name));
			for (int p : left_p)  steps[p].res_ptr = me;
			for (int p : right_p) steps[p].res_ptr = me;
			return {me};
		}
		case K::Call: {
			std::vector<int> pend;
			for (const auto& a : node.args) {
				auto sub = compileNode(*a, steps);
				pend.insert(pend.end(), sub.begin(), sub.end());
			}
			int me = appendStep(steps, kind_from_opname(node.name));
			for (int p : pend) steps[p].res_ptr = me;
			return {me};
		}
	}
	throw std::runtime_error("未知 AST 节点类型");
}

static std::vector<CalcStep> tokenizeToSteps_internal(const std::string& expr) {
	Tokenizer tz; auto toks = tz.lex(expr);
	Parser ps; ps.toks = std::move(toks);
	auto ast = ps.parseExpr(0);
	if (ps.peekType() != Token::Type::Eof) throw std::runtime_error("多余的输入");

	std::vector<CalcStep> steps;
	auto root_pend = compileNode(*ast, steps);
	for (int p : root_pend) steps[p].res_ptr = -1; // 顶层输出
	return steps;
}

// ===== 对外 API 实现 =====

std::vector<CalcStep> build_calcflow_from_formula(const std::string& expr) {
	return tokenizeToSteps_internal(expr);
}

static std::string to_json_number(double v) {
	std::ostringstream oss; oss.setf(std::ios::fmtflags(0), std::ios::floatfield);
	oss.precision(17);
	oss << v;
	return oss.str();
}

std::string calcflow_to_json(const std::vector<CalcStep>& steps) {
	std::ostringstream oss;
	oss << "{\n  \"steps\": [\n";
	for (size_t i = 0; i < steps.size(); ++i) {
		const auto& st = steps[i];
		oss << "    {\n";
		oss << "      \"op\": \"" << opname_from_kind(st.op) << "\",\n";
		if (!st.var_name.empty()) {
			oss << "      \"var\": \"" << st.var_name << "\",\n";
		}
		oss << "      \"res_ptr\": " << st.res_ptr << ",\n";
		oss << "      \"args\": [";
		for (size_t j = 0; j < st.args.size(); ++j) {
			const auto& c = st.args[j];
			oss << "{\"re\": " << to_json_number(c.real())
				<< ", \"im\": " << to_json_number(c.imag()) << "}";
			if (j + 1 < st.args.size()) oss << ", ";
		}
		oss << "]\n    }";
		if (i + 1 < steps.size()) oss << ",";
		oss << "\n";
	}
	oss << "  ]\n}";
	return oss.str();
}

// 极简 JSON 读取器（仅支持本结构）
struct JsonReader {
	const std::string& s; size_t i{0};
	JsonReader(const std::string& str): s(str) {}
	void ws(){ while(i<s.size() && (unsigned char)s[i]<= ' ') ++i; }
	void expect(char c){ ws(); if(i>=s.size()||s[i]!=c) throw std::runtime_error("JSON 语法错误: 期待字符"); ++i; }
	bool consume(char c){ ws(); if(i<s.size() && s[i]==c){ ++i; return true;} return false; }
	std::string parse_string(){ ws(); if(s[i] != '"') throw std::runtime_error("JSON: 期待字符串"); ++i; std::string r; while(i<s.size() && s[i] != '"'){ r.push_back(s[i++]); } if(i>=s.size()) throw std::runtime_error("JSON: 字符串未闭合"); ++i; return r; }
	double parse_number(){ ws(); size_t st=i; while(i<s.size() && (std::isdigit((unsigned char)s[i])||s[i]=='-'||s[i]=='+'||s[i]=='.'||s[i]=='e'||s[i]=='E')) ++i; return std::strtod(s.c_str()+st,nullptr); }
	int parse_int(){ return static_cast<int>(parse_number()); }
};

std::vector<CalcStep> calcflow_from_json(const std::string& json) {
	JsonReader R(json);
	std::vector<CalcStep> out;
	R.ws(); R.expect('{');
	// 读取键值对（只关心 steps）
	while (true) {
		R.ws();
		if (R.consume('}')) break;
		std::string key = R.parse_string();
		R.ws(); R.expect(':');
		if (key == "steps") {
			R.ws(); R.expect('[');
			// 读取 steps 数组
			bool first = true;
			while (true) {
				R.ws();
				if (R.consume(']')) break;
				if (!first) { /* expecting comma previously consumed by loop */ }
				first = false;
				// 读取单个 step 对象
				R.ws(); R.expect('{');
				CalcStep st;
				// 缺省字段
				st.res = Complex{0.0,0.0}; st.res_ptr = -1; st.args.clear(); st.var_name.clear(); st.op = OpKind::Push;
				// 键值对
				bool objdone=false; bool firstkv=true;
				while (!objdone) {
					R.ws();
					if (R.consume('}')) break;
					std::string k = R.parse_string();
					R.ws(); R.expect(':');
					if (k == "op") {
						std::string v = R.parse_string();
						st.op = kind_from_opname(v);
					} else if (k == "var") {
						st.var_name = R.parse_string();
					} else if (k == "res_ptr") {
						st.res_ptr = R.parse_int();
					} else if (k == "args") {
						R.ws(); R.expect('[');
						bool firstArg = true;
						while (true) {
							R.ws();
							if (R.consume(']')) break;
							if (!firstArg) { /* comma handled by structure */ }
							firstArg = false;
							R.ws(); R.expect('{');
							double re=0.0, im=0.0; bool got_re=false, got_im=false;
							while (true) {
								R.ws();
								if (R.consume('}')) break;
								std::string kk = R.parse_string();
								R.ws(); R.expect(':');
								if (kk == "re") { re = R.parse_number(); got_re=true; }
								else if (kk == "im") { im = R.parse_number(); got_im=true; }
								R.ws(); (void)R.consume(',');
							}
							st.args.emplace_back(re, im);
							R.ws(); (void)R.consume(',');
						}
					} else {
						// 跳过未知字段的简单值/字符串/数组/对象：这里简单跳过字符串或数字
						R.ws();
						if (R.consume('"')) { // 简单跳字符串
							while (R.i < json.size() && json[R.i] != '"') ++R.i; if (R.i<json.size()) ++R.i;
						} else if (R.consume('[')) {
							// 粗略跳过数组，直到匹配到 ']'（不处理嵌套复杂情况，因为我们生成的 JSON 不会有更深层）
							int depth=1; while (R.i < json.size() && depth>0) { if (json[R.i]=='[') ++depth; else if (json[R.i]==']') --depth; ++R.i; }
						} else if (R.consume('{')) {
							int depth=1; while (R.i < json.size() && depth>0) { if (json[R.i]=='{') ++depth; else if (json[R.i]=='}') --depth; ++R.i; }
						} else {
							// 数字/布尔/null 粗略跳过
							(void)R.parse_number();
						}
					}
					R.ws(); (void)R.consume(',');
				}
				out.push_back(std::move(st));
				R.ws(); (void)R.consume(',');
			}
		} else {
			// 跳过不关心的键
			R.ws();
			if (R.consume('"')) { while (R.i < json.size() && json[R.i] != '"') ++R.i; if (R.i<json.size()) ++R.i; }
			else if (R.consume('{')) { int d=1; while (R.i < json.size() && d>0) { if (json[R.i]=='{') ++d; else if (json[R.i]=='}') --d; ++R.i; } }
			else if (R.consume('[')) { int d=1; while (R.i < json.size() && d>0) { if (json[R.i]=='[') ++d; else if (json[R.i]==']') --d; ++R.i; } }
			else { (void)R.parse_number(); }
		}
		R.ws(); (void)R.consume(',');
	}
	return out;
}

bool save_calcflow_to_json(const std::vector<CalcStep>& steps, const std::string& path) {
	std::ofstream ofs(path, std::ios::binary);
	if (!ofs) return false;
	std::string j = calcflow_to_json(steps);
	ofs.write(j.data(), static_cast<std::streamsize>(j.size()));
	return static_cast<bool>(ofs);
}

std::vector<CalcStep> load_calcflow_from_json(const std::string& path) {
	std::ifstream ifs(path, std::ios::binary);
	if (!ifs) throw std::runtime_error("无法打开文件: " + path);
	std::ostringstream oss; oss << ifs.rdbuf();
	return calcflow_from_json(oss.str());
}

Complex evaluate_calcflow(const std::vector<CalcStep>& steps,
						  const std::unordered_map<std::string, Complex>& vars) {
	Calc c(steps);
	return c.calculate(vars);
}

