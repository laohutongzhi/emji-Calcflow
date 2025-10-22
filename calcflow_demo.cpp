#include "calcflow.hpp"
#include <iostream>
#include <unordered_map>
#include <vector>
#include <string>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4996)
#endif

int main() {
    auto eval = [](const std::string& s) -> Complex {
        auto steps = build_calcflow_from_formula(s);
        return evaluate_calcflow(steps, {});
    };

    std::vector<std::string> samples = {
        "add(1,2,sin(pi/2))",  // 4
        "1+2*sin(pi/2)",       // 3
        "pow(2,3) + 4",        // 12
        "log(e, e^2)",         // 2
        "abs(-3) + abs(-4i)",  // 7
        "(1+2i)^2 + 2^2",      // 1+4i
        "cos(0)+sin(0)+tan(0)",
        "3^2^2",               // 81
        "sub(10, 1, 2, 3)",    // 4
        "div(100, 2, 5)",      // 10
    };

    for (const auto& s : samples) {
        try {
            auto v = eval(s);
            std::cout << s << " = " << v << "\n";
        } catch (const std::exception& e) {
            std::cout << s << " -> 错误: " << e.what() << "\n";
        }
    }

    // 变量示例：x^2 + y^2（并演示 JSON 存取）
    try {
        auto steps = build_calcflow_from_formula("x^2 + y^2");
        // 保存到 JSON
        (void)save_calcflow_to_json(steps, "calcflow_example.json");
        // 从 JSON 读取
        auto steps2 = load_calcflow_from_json("calcflow_example.json");
        std::unordered_map<std::string, Complex> vars1{{"x", {3.0,0.0}}, {"y", {4.0,0.0}}};
        std::cout << "x=3,y=4 => " << evaluate_calcflow(steps2, vars1) << "\n";
        std::unordered_map<std::string, Complex> vars2{{"x", {1.0,2.0}}, {"y", {2.0,0.0}}};
        std::cout << "x=1+2i,y=2 => " << evaluate_calcflow(steps2, vars2) << "\n";
    } catch (const std::exception& e) {
        std::cout << "变量示例错误: " << e.what() << "\n";
    }

    return 0;
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif
