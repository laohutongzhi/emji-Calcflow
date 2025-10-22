#include "calcflow.hpp"

// 说明：
//  - 本文件提供“中文名”的 API 封装，底层直接调用 calcflow.hpp 中的英文 API。
//  - 目的是便于中文读者快速理解/演示；生产环境仍建议使用英文 API 与标识符。
//  - 源文件需保存为 UTF-8 编码。

// —— 中文类型别名（仅此文件内可见） ——
using 复数 = Complex;                         // complex<double>
using 步骤 = CalcStep;                        // 计算步骤结构体
template<typename T> using 向量 = std::vector<T>;
template<typename K, typename V> using 字典 = std::unordered_map<K, V>;

// —— 操作符枚举中文映射（硬翻译） ——
// 说明：这些常量仅作“中文名”映射，真实语义与 OpKind 完全一致。
constexpr OpKind 推入   = OpKind::Push;
constexpr OpKind 加     = OpKind::Add;
constexpr OpKind 减     = OpKind::Sub;
constexpr OpKind 乘     = OpKind::Mul;
constexpr OpKind 除以   = OpKind::Div;
constexpr OpKind 幂     = OpKind::Pow;
constexpr OpKind 对数   = OpKind::Log;
constexpr OpKind 相反数 = OpKind::Neg;
constexpr OpKind 正弦   = OpKind::Sin;
constexpr OpKind 余弦   = OpKind::Cos;
constexpr OpKind 正切   = OpKind::Tan;
constexpr OpKind 指数   = OpKind::Exp;
constexpr OpKind 绝对值 = OpKind::Abs;
constexpr OpKind 变量   = OpKind::Var;

// —— 操作名 <-> 枚举 中文包装 ——
inline const char* 操作名_自枚举(OpKind k) { return opname_from_kind(k); }
inline OpKind 枚举_自操作名(const std::string& 名) { return kind_from_opname(名); }

// —— 构建/序列化/计算 中文 API ——
inline 向量<步骤> 从公式构建计算流(const std::string& 表达式) {
    return build_calcflow_from_formula(表达式);
}

inline std::string 计算流转JSON(const 向量<步骤>& 步骤们) {
    return calcflow_to_json(步骤们);
}

inline 向量<步骤> 从JSON解析计算流(const std::string& 文本) {
    return calcflow_from_json(文本);
}

inline bool 保存计算流到JSON(const 向量<步骤>& 步骤们, const std::string& 路径) {
    return save_calcflow_to_json(步骤们, 路径);
}

inline 向量<步骤> 从JSON读取计算流(const std::string& 路径) {
    return load_calcflow_from_json(路径);
}

inline 复数 计算(const 向量<步骤>& 步骤们, const 字典<std::string, 复数>& 变量表 = {}) {
    return evaluate_calcflow(步骤们, 变量表);
}

// —— 备注 ——
// 若需要进一步“全中文化”的结构体/字段命名，可参考下列示意（仅注释）：
// struct 步骤中文 {
//     OpKind 操作;                // 对应 CalcStep::op
//     std::vector<复数> 参数们;    // 对应 CalcStep::args
//     std::string 变量名;         // 对应 CalcStep::var_name
//     复数 结果;                   // 对应 CalcStep::res
//     int 结果投递到;             // 对应 CalcStep::res_ptr
// };
