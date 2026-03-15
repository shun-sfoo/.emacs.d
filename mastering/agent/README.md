# 本地 AI Agent - 基于 LangGraph + Ollama

一个使用 LangGraph 框架构建的本地 AI Agent，集成 Ollama 和 Qwen3.5:0.8b 模型。

## 环境要求

- Python 3.10+
- Ollama 已安装并运行
- Qwen3.5:0.8b 模型已下载

## 快速开始

### 1. 安装依赖

```bash
pip install -r requirements.txt
```

### 2. 确保 Ollama 服务运行

```bash
# 检查模型是否已安装
ollama list

# 启动 Ollama 服务（如果未运行）
ollama serve
```

### 3. 运行 Agent

```bash
# 交互式对话模式
python agent.py
```

或使用示例程序:

```bash
python example.py
```

## 项目结构

```
.
├── agent.py          # 主程序 - LangGraph Agent 实现
├── example.py        # 使用示例
├── requirements.txt  # Python 依赖
└── README.md         # 本文档
```

## 功能特性

### 1. 基础对话
与 AI 进行自然语言对话，保持上下文记忆。

### 2. 工具调用
Agent 可以调用以下工具:
- **calculator**: 数学计算 (如 `2+2`, `sqrt(16)`, `sin(0.5)`)
- **get_current_time**: 获取当前时间
- **get_current_date**: 获取当前日期
- **fetch_webpage**: 获取网页内容 (提供URL)
- **translate_to_chinese**: 英文翻译成中文
- **read_file**: 读取本地文件
- **list_directory**: 列出目录内容

### 3. 持久化存储
- SQLite 数据库存储对话历史
- 支持恢复历史会话
- 自动保存对话记录

### 4. 多轮对话
使用 LangGraph 维护对话状态，支持多轮交互。

## 使用示例

```
你: 你好
AI: 你好！有什么我可以帮助你的吗？

你: 帮我计算 123 * 456
AI: 计算结果: 56088

你: 现在几点钟?
AI: 当前时间是: 2026年03月10日 15:16:32

你: 今天是几号?
AI: 今天是: 2026年03月10日
```

## 核心概念

### LangGraph 工作流

```
用户输入 → Agent节点(LLM决策) → [需要工具?] → 是 → 工具节点 → 返回Agent节点
                                    ↓ 否
                                  结束
```

### 状态管理

AgentState 包含:
- `messages`: 对话消息历史

## 自定义扩展

### 添加新工具

1. 在 `agent.py` 中定义工具函数
2. 在 `get_tools()` 中注册工具
3. 重新运行程序

示例:
```python
# 定义工具
def my_tool(param: str) -> str:
    return f"结果: {param}"

# 注册工具
tools = [
    {
        "name": "my_tool",
        "description": "工具描述",
        "parameters": {"type": "object", "properties": {"param": {"type": "string"}}}
    },
]
```

### 更换模型

在创建 ChatOllama 实例时修改模型名称:
```python
model = ChatOllama(
    model="qwen3.5:1.8b",  # 更换为其他模型
    temperature=0.7,
)
```

## 常见问题

### Q: 模型下载很慢怎么办?
A: 可以使用更小的模型，如 `qwen3.5:0.8b` (已使用) 或 `llama3.2:1b`

### Q: 工具调用失败怎么办?
A: 检查模型是否支持工具调用功能，较小模型可能不支持

### Q: 如何退出程序?
A: 输入 `quit` 或 `exit` 或 `q`

## 参考资料

- [LangGraph 官方文档](https://langchain-ai.github.io/langgraph/)
- [LangChain Ollama](https://python.langchain.com/docs/integrations/chat/ollama/)
- [Ollama 官方文档](https://github.com/ollama/ollama)
