# LangGraph 完全指南

本文档详细讲解 LangGraph 的核心概念、架构设计，并通过项目代码作为示例进行说明。

---

## 目录

1. [什么是 LangGraph？](#1-什么是-langgraph)
2. [核心概念](#2-核心概念)
3. [工作流程详解](#3-工作流程详解)
4. [代码逐行解析](#4-代码逐行解析)
5. [状态管理](#5-状态管理)
6. [工具调用机制](#6-工具调用机制)
7. [条件边与循环](#7-条件边与循环)
8. [最佳实践](#8-最佳实践)

---

## 1. 什么是 LangGraph？

### 1.1 定义

**LangGraph** 是 LangChain 团队开发的一个用于构建有状态、多actor AI 应用的框架。它的核心思想是将 AI 应用建模为**有向图**（Directed Graph）。

### 1.2 为什么需要 LangGraph？

传统的 AI 应用（如简单对话）是：
```
用户输入 → LLM → 输出 → 结束
```

但实际需求更复杂：
- 需要多轮对话保持上下文
- 需要根据不同情况执行不同逻辑
- 需要调用外部工具
- 需要循环执行（工具调用后再次调用 LLM）

LangGraph 就是来解决这些问题的。

### 1.3 核心特性

| 特性 | 说明 |
|------|------|
| **有状态** | 可以在节点之间传递和更新状态 |
| **循环支持** | 支持 LLM → 工具 → LLM 的循环 |
| **可持久化** | 支持保存和恢复图状态 |
| **可调试** | 支持检查点、重新运行 |

---

## 2. 核心概念

### 2.1 图 (Graph)

在 LangGraph 中，你的 AI 应用被建模为一个**图**：

```
    ┌─────────┐
    │  START  │
    └────┬────┘
         │
         ▼
    ┌─────────┐
    │  agent  │◄──────┐
    └────┬────┘       │
         │            │
    ┌────┴────┐       │
    │ needs   │       │
    │ tools?  │       │
    └────┬────┘       │
         │            │
    ┌────┴────┐    ┌──┴────┐
    │   NO    │    │ YES   │
    └────┬────┘    └────┬──┘
         │               │
         ▼               ▼
    ┌─────────┐    ┌─────────┐
    │  END    │    │ tools   │
    └─────────┘    └────┬────┘
                        │
                        └────► (返回 agent)
```

### 2.2 节点 (Node)

**节点**是图中的基本执行单元，每个节点：
- 接收输入（当前状态）
- 执行特定逻辑
- 返回输出（更新后的状态）

在代码中，节点就是一个**函数**：

```python
def my_node(state: AgentState) -> AgentState:
    # 处理逻辑
    return {"key": "new_value"}
```

### 2.3 边 (Edge)

**边**连接节点，定义执行流程：

| 类型 | 说明 |
|------|------|
| **普通边** | A → B，固定顺序执行 |
| **条件边** | 根据条件选择下一个节点 |

### 2.4 状态 (State)

**状态**是在整个图中流动的数据。LangGraph 使用 TypedDict 定义状态类型：

```python
class AgentState(TypedDict):
    messages: Annotated[list, add_messages]  # 消息历史
    count: int                                 # 计数器
```

`Annotated[list, add_messages]` 表示：列表类型 + 使用 `add_messages` 进行增量更新。

---

## 3. 工作流程详解

### 3.1 我们的项目架构

```
用户输入
    │
    ▼
┌─────────────────────┐
│     agent 节点       │ ◄── 调用 LLM (Qwen3.5)
│  (LLM 决策/推理)     │
└─────────┬───────────┘
          │
          │ LLM 返回内容
          ▼
    ┌───────────┐
    │ 需要工具? │ ◄── 检查 tool_calls
    └─────┬─────┘
          │
    ┌─────┴─────┐
    │           │
   YES         NO
    │           │
    ▼           ▼
┌────────┐  ┌────────┐
│ tools  │  │  END   │
│ 节点   │  │ 结束   │
└───┬────┘  └────────┘
    │
    │ 工具结果
    └──────► (返回 agent 节点，继续循环)
```

### 3.2 完整执行流程

```
1. 用户: "帮我计算 123 * 456"
   ↓
2. messages = [SystemMessage, HumanMessage("帮我计算 123 * 456")]
   ↓
3. agent 节点调用 LLM
   ↓
4. LLM 决定调用 calculator 工具
   ↓
5. 返回 AIMessage(tool_calls=[{name: "calculator", args: {expression: "123*456"}}])
   ↓
6. should_continue() 检测到 tool_calls，返回 "tools"
   ↓
7. tools 节点执行 calculator，得到 "计算结果: 56088"
   ↓
8. 工具结果作为 ToolMessage 添加到 messages
   ↓
9. 回到 agent 节点，LLM 根据工具结果生成最终回复
   ↓
10. 返回最终回复 "123 * 456 = 56088"
   ↓
11. should_continue() 检测无 tool_calls，返回 END
   ↓
12. 流程结束
```

---

## 4. 代码逐行解析

### 4.1 状态定义

```python
# agent.py 第 39-43 行
class AgentState(TypedDict):
    """
    Agent 状态定义 - 使用 TypedDict 定义状态的类型提示
    
    LangGraph 使用这个状态来在各个节点之间传递数据。
    每次状态更新时，add_messages 会将新消息追加到消息列表中。
    """
    # messages 字段类型: 
    # - Annotated[Sequence[BaseMessage], add_messages]
    # - Sequence[BaseMessage]: 消息列表
    # - add_messages: 增量更新函数，新消息会追加而非覆盖
    messages: Annotated[Sequence[BaseMessage], add_messages]
```

**关键点**：
- `TypedDict` 定义状态结构
- `Annotated` 添加元信息（这里是 `add_messages` 修饰器）
- `add_messages` 确保新消息追加而非覆盖

### 4.2 工具定义

```python
# agent.py 第 45-55 行
@tool
def calculator(expression: str) -> str:
    """
    数学计算工具。用于执行数学表达式计算。
    
    @tool 装饰器:
    - 自动提取函数签名生成工具 schema
    - 自动处理参数验证
    - 使函数可以被 LLM 调用
    """
    try:
        allowed_names = {...}  # 白名单安全
        result = eval(expression, {"__builtins__": {}}, allowed_names)
        return f"计算结果: {result}"
    except Exception as e:
        return f"计算错误: {str(e)}"
```

**@tool 装饰器的作用**：
1. 解析函数签名和 docstring
2. 生成 OpenAI 格式的工具 schema
3. 添加 `.invoke()` 方法用于调用

### 4.3 Agent 节点

```python
# agent.py 第 133-142 行
def create_agent_node(model):
    """创建 Agent 节点函数"""
    
    def agent_node(state: AgentState) -> AgentState:
        """
        Agent 处理节点 - 调用 LLM 进行推理
        
        这是 LangGraph 图中的核心节点，负责:
        1. 接收当前状态（包含消息历史）
        2. 调用 LLM 进行推理
        3. 如果 LLM 返回工具调用请求，生成 tool_calls
        4. 返回更新后的状态
        """
        # 1. 从状态中获取消息历史
        messages = state["messages"]
        
        # 2. 调用模型进行推理
        # bind_tools(tools=get_tool_list()) 关键步骤:
        # - 将工具列表绑定到模型
        # - 使模型能够生成工具调用请求
        # - 模型会在需要时返回 tool_calls
        response = model.bind_tools(tools=get_tool_list()).invoke(messages)
        
        # 3. 返回更新后的状态
        # add_messages 会将新消息(AIMessage)追加到消息列表
        return {"messages": [response]}
    
    return agent_node
```

**核心要点**：
- `model.bind_tools()` 绑定工具，使 LLM 知道有哪些工具可用
- LLM 决定是否调用工具（通过返回 `tool_calls`）
- 返回新消息，状态自动追加

### 4.4 工具执行节点

```python
# agent.py 第 145-177 行
def create_tool_node():
    """创建工具执行节点"""
    
    def tool_node(state: AgentState) -> AgentState:
        """
        工具执行节点 - 执行 Agent 请求的工具
        
        处理过程:
        1. 获取最后一条消息（应该是 AI 的工具调用请求）
        2. 解析工具调用参数
        3. 执行相应的工具函数
        4. 将工具结果作为 ToolMessage 添加到状态
        """
        # 1. 获取最后一条消息
        last_message = state["messages"][-1]
        
        # 2. 检查是否有工具调用请求
        # tool_calls 包含: [{name: "calculator", args: {...}, id: "..."}]
        tool_calls = getattr(last_message, "tool_calls", None)
        if not tool_calls:
            return state  # 没有工具调用，直接返回
        
        # 3. 遍历并执行每个工具调用
        tool_results = []
        for tool_call in tool_calls:
            tool_name = tool_call["name"]      # 工具名称
            tool_args = tool_call.get("args", {})  # 工具参数
            
            # 4. 查找并执行工具函数
            tool_func = None
            for t in get_tool_list():
                if t.name == tool_name:
                    tool_func = t
                    break
            
            if tool_func:
                try:
                    # 调用工具
                    result = tool_func.invoke(tool_args)
                    # 5. 创建 ToolMessage 存储结果
                    tool_results.append(
                        ToolMessage(
                            content=str(result),  # 工具返回内容
                            tool_call_id=tool_call.get("id", "")  # 关联 ID
                        )
                    )
                except Exception as e:
                    tool_results.append(ToolMessage(content=f"工具执行错误: {str(e)}"))
            else:
                tool_results.append(ToolMessage(content=f"未知工具: {tool_name}"))
        
        # 6. 返回工具结果（会追加到消息历史）
        return {"messages": tool_results}
    
    return tool_node
```

**关键点**：
- 工具执行后生成 `ToolMessage`
- `tool_call_id` 关联原始调用
- 结果返回后会追加到消息历史，供下一轮 LLM 使用

### 4.5 构建工作流图

```python
# agent.py 第 193-228 行
def create_agent_graph(model):
    """创建 Agent 工作流图"""
    
    # 1. 创建状态图
    workflow = StateGraph(AgentState)
    
    # 2. 添加节点
    workflow.add_node("agent", create_agent_node(model))   # LLM 决策节点
    workflow.add_node("tools", create_tool_node())       # 工具执行节点
    
    # 3. 设置入口点
    workflow.set_entry_point("agent")
    
    # 4. 定义条件边函数
    def should_continue(state: AgentState) -> str:
        """
        判断是否继续执行（是否需要调用工具）
        
        核心逻辑:
        - 如果 LLM 返回了 tool_calls → 需要执行工具 → 返回 "tools"
        - 否则 → 流程结束 → 返回 END
        """
        messages = state["messages"]
        last_message = messages[-1]
        
        # 检查是否有工具调用
        tool_calls = getattr(last_message, "tool_calls", None)
        return "tools" if tool_calls else "__end__"
    
    # 5. 添加条件边
    # 从 agent 节点出发，根据 should_continue 结果决定下一步
    workflow.add_conditional_edges(
        "agent",           # 源节点
        should_continue,  # 判断函数
        {                  # 映射表
            "tools": "tools",   # 返回 "tools" → 跳转到 tools 节点
            "__end__": END       # 返回 "__end__" → 结束
        }
    )
    
    # 6. 添加普通边
    # tools 执行完后，返回 agent 节点（继续循环）
    workflow.add_edge("tools", "agent")
    
    # 7. 编译图
    return workflow.compile()
```

**图结构总结**：

```
                    ┌─────────────┐
                    │   START     │
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
              ┌────►│    agent    │◄────┐
              │     └──────┬──────┘     │
              │            │            │
              │     tool_calls?          │
              │            │            │
              │     ┌──────┴──────┐     │
              │     │              │     │
             YES    │             NO    │
              │     ▼              │     │
              │  ┌────────┐       │     │
              │  │ tools  │───────┘     │
              │  └────────┘             │
              │     │                   │
              └─────┴───────────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │     END     │
                    └─────────────┘
```

---

## 5. 状态管理

### 5.1 状态更新机制

LangGraph 使用 **增量更新** 模式：

```python
# 假设当前状态
state = {"messages": [SystemMessage("你好"), HumanMessage("你好")]}
#                     ↑ index=0                    ↑ index=1

# agent_node 返回
return {"messages": [AIMessage("你好，有什么可以帮您？")]}
#                     ↑ 新消息

# LangGraph 自动合并（使用 add_messages）
# 结果: [SystemMessage, HumanMessage, AIMessage]
```

### 5.2 add_messages 详解

```python
from langgraph.graph import add_messages

# add_messages 本质是一个函数:
def add_messages(existing: list, new: list) -> list:
    """将新消息追加到现有列表"""
    return existing + new

# 使用 Annotated 指定:
messages: Annotated[Sequence[BaseMessage], add_messages]
```

### 5.3 自定义状态字段

你可以在状态中添加任意字段：

```python
class AgentState(TypedDict):
    messages: Annotated[Sequence[BaseMessage], add_messages]
    count: int  # 对话轮次
    context: dict  # 自定义上下文
```

---

## 6. 工具调用机制

### 6.1 LLM 如何决定调用工具？

当使用 `bind_tools()` 绑定工具后，LLM 的输出会包含：

```python
# LLM 返回
AIMessage(
    content="...",              # 可选的文本回复
    tool_calls=[                # 工具调用请求
        {
            "name": "calculator",      # 工具名
            "args": {"expression": "123*456"},  # 参数
            "id": "call_abc123"        # 调用 ID
        }
    ]
)
```

### 6.2 工具调用的完整流程

```
┌─────────────────────────────────────────────────────────────┐
│                        用户输入                              │
│              "帮我计算 15 + 27 等于多少"                      │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                     1. agent 节点                            │
│  model.bind_tools(tools).invoke(messages)                   │
│                                                              │
│  LLM 分析用户意图:                                           │
│  "用户需要计算，应该调用 calculator 工具"                      │
│                                                              │
│  返回: AIMessage(                                            │
│    tool_calls=[{                                            │
│      "name": "calculator",                                  │
│      "args": {"expression": "15+27"}                        │
│    }]                                                        │
│  )                                                           │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   2. should_continue                         │
│  检测到 tool_calls，返回 "tools"                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                     3. tools 节点                            │
│  tool_func.invoke({"expression": "15+27"})                  │
│                                                              │
│  执行: calculator(expression="15+27")                       │
│  返回: "计算结果: 42"                                        │
│                                                              │
│  生成: ToolMessage(content="计算结果: 42")                   │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                     4. 循环回到 agent                        │
│  messages = [System, Human, AIMessage(tool_calls),          │
│              ToolMessage(计算结果)]                          │
│                                                              │
│  再次调用 LLM:                                               │
│  "工具返回了结果 42，我可以告诉用户了"                         │
│                                                              │
│  返回: AIMessage(content="15 + 27 = 42")                   │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   5. should_continue                         │
│  无 tool_calls，返回 "__end__"                               │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                       6. 结束                                │
│  最终回复: "15 + 27 = 42"                                   │
└─────────────────────────────────────────────────────────────┘
```

---

## 7. 条件边与循环

### 7.1 条件边

条件边根据条件函数的返回值决定下一步：

```python
workflow.add_conditional_edges(
    "agent",              # 源节点
    should_continue,     # 条件函数
    {                    # 返回值映射
        "tools": "tools",
        "__end__": END
    }
)
```

### 7.2 循环实现

LangGraph 通过 **边回到之前节点** 实现循环：

```python
workflow.add_edge("tools", "agent")  # tools 执行完回到 agent
```

这创建了 `tools → agent` 的循环，允许：
- 工具 → LLM → 工具 → LLM → ... （多次工具调用）
- 每次工具执行完后，LLM 可以再次决定是否需要更多工具

### 7.3 终止条件

循环在以下情况终止：
- `should_continue` 返回 `END`
- 返回 `"__end__"`（LangGraph 约定）
- 达到最大循环次数（需要手动实现）

---

## 8. 最佳实践

### 8.1 状态设计

```python
# ✅ 推荐: 清晰的状态结构
class AgentState(TypedDict):
    messages: Annotated[Sequence[BaseMessage], add_messages]
    current_step: str

# ❌ 避免: 过于复杂的状态
class AgentState(TypedDict):
    messages: Annotated[Sequence[BaseMessage], add_messages]
    step1_result: Any
    step2_result: Any
    step3_result: Any
    # ... 更多字段
```

### 8.2 节点函数

```python
# ✅ 推荐: 简洁的节点函数
def agent_node(state: AgentState) -> AgentState:
    messages = state["messages"]
    response = model.invoke(messages)
    return {"messages": [response]}

# ❌ 避免: 节点函数过于复杂
def agent_node(state: AgentState) -> AgentState:
    # 100 行处理逻辑...
    # 尽量拆分到多个节点
```

### 8.3 工具定义

```python
# ✅ 推荐: 清晰的工具描述
@tool
def calculator(expression: str) -> str:
    """数学计算器。执行基本数学运算。
    
    Args:
        expression: 数学表达式，如 "2+2" 或 "sqrt(16)"
    
    Returns:
        计算结果
    """
    return str(eval(expression))

# ❌ 避免: 模糊的描述
@tool
def calc(x):
    """计算"""
    return eval(x)
```

### 8.4 错误处理

```python
def tool_node(state: AgentState) -> AgentState:
    try:
        # 工具执行逻辑
        result = tool.invoke(args)
    except Exception as e:
        # 返回错误信息，而不是抛出异常
        result = f"错误: {str(e)}"
    
    return {"messages": [ToolMessage(content=result)]}
```

---

## 9. 扩展学习

### 9.1 更多 LangGraph 特性

| 特性 | 说明 |
|------|------|
| **Checkpointer** | 保存和恢复图状态，实现暂停/恢复 |
| **Memory** | 持久化状态管理 |
| **Pregel** | LangGraph 底层执行引擎 |
| **Command** | 节点内控制流程 |

### 9.2 参考资源

- 官方文档: https://langchain-ai.github.io/langgraph/
- GitHub: https://github.com/langchain-ai/langgraph
- 示例: https://github.com/langchain-ai/langgraph/tree/main/examples

---

## 10. 总结

LangGraph 的核心价值：

1. **图模型** - 将 AI 应用建模为有向图，清晰表达流程
2. **状态流** - 状态在节点间流动和累积
3. **循环支持** - 天然支持 LLM → 工具 → LLM 的循环
4. **可组合** - 节点和边可灵活组合

掌握这些概念，你就可以构建复杂的 AI 应用，如：
- 多轮对话 Agent
- 工具调用 Agent
- RAG 系统
- Agent 工作流
- 等等

祝学习愉快！
