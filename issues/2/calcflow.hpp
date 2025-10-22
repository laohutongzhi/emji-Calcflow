#pragma once

#include <complex>
#include <string>
#include <vector>
#include <unordered_map>

using Complex = std::complex<double>;

// 与实现保持一致的操作符集合
enum class OpKind {
    Push,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Log,
    Neg,
    Sin,
    Cos,
    Tan,
    Exp,
    Abs,
    Var,
};

// 执行步骤（序列化/反序列化需要此结构）
struct CalcStep {
    OpKind op{OpKind::Push};
    std::vector<Complex> args{};   // 初始参数（字面量、常量推入）+ 运行时收集的子结果
    std::string var_name{};        // 仅 Var 使用
    Complex res{0.0, 0.0};         // 运行时结果（JSON 可不必保存）
    int res_ptr{-1};               // 结果投递目标步骤索引（-1 为最终输出）
};

// 1) 从公式解析得到 CalcStep 流
std::vector<CalcStep> build_calcflow_from_formula(const std::string& expr);

// 2) 将 CalcStep 流转为 JSON 字符串 / 从 JSON 字符串恢复
std::string calcflow_to_json(const std::vector<CalcStep>& steps);
std::vector<CalcStep> calcflow_from_json(const std::string& json);

// 3) 保存/读取 JSON 文件
bool save_calcflow_to_json(const std::vector<CalcStep>& steps, const std::string& path);
std::vector<CalcStep> load_calcflow_from_json(const std::string& path);

// 4) 计算结果（可传变量环境）
Complex evaluate_calcflow(const std::vector<CalcStep>& steps,
                          const std::unordered_map<std::string, Complex>& vars = {});

// 可选：操作符名字互转（方便外部查看/调试）
const char* opname_from_kind(OpKind k);
OpKind kind_from_opname(const std::string& name);
