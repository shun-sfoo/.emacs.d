"""
本地 AI Agent - 基于 LangGraph 和 Ollama (Qwen3.5:0.8b)

该项目演示了如何使用 LangGraph 框架构建一个具有工具调用能力的本地 AI Agent。
Agent 使用 Ollama 在本地运行 Qwen3.5:0.8b 模型，并通过 LangGraph 实现工作流编排。

主要功能:
1. 基础对话 - 与 AI 进行自然语言对话
2. 工具调用 - Agent 可以调用外部工具完成任务
3. 文件操作 - 读取文件和列出目录
"""

import math
import requests
import os
import io
import base64
import json

from typing import TypedDict, Annotated, Sequence
from typing_extensions import TypedDict

from langgraph.graph import StateGraph, END
from langgraph.graph import add_messages

from langchain_core.messages import (
    BaseMessage,
    HumanMessage,
    AIMessage,
    SystemMessage,
    ToolMessage,
)
from langchain_core.tools import tool

from langchain_ollama import ChatOllama


def display_qrcode_in_kitty(data: str) -> str:
    """在 kitty 终端中显示二维码"""
    import qrcode

    import sys
    import io

    qr = qrcode.QRCode(box_size=10, border=1)
    qr.add_data(data)
    qr.make(fit=True)
    img = qr.make_image()

    buffer = io.BytesIO()
    img.save(buffer, "PNG")
    image_data = base64.b64encode(buffer.getvalue()).decode("ascii")

    pc = f"\033_Ga=T,f=100;{image_data}\033\\"
    sys.stdout.buffer.write(pc.encode("ascii"))
    sys.stdout.buffer.flush()

    return "二维码已在终端显示"


# =============================================================================
# 第一部分: 定义 Agent 状态 (State)
# =============================================================================


class AgentState(TypedDict):
    """Agent 状态定义"""

    messages: Annotated[Sequence[BaseMessage], add_messages]


# =============================================================================
# 第二部分: 工具定义 (Tools)
# =============================================================================


@tool
def calculator(expression: str) -> str:
    """数学计算工具。用于执行数学表达式计算。"""
    try:
        allowed_names = {
            "abs": abs,
            "sqrt": math.sqrt,
            "pow": pow,
            "sin": math.sin,
            "cos": math.cos,
            "tan": math.tan,
            "log": math.log,
            "pi": math.pi,
            "e": math.e,
            "ceil": math.ceil,
            "floor": math.floor,
        }
        result = eval(expression, {"__builtins__": {}}, allowed_names)
        return f"计算结果: {result}"
    except Exception as e:
        return f"计算错误: {str(e)}"


@tool
def get_current_time() -> str:
    """获取当前时间工具。返回当前的日期和时间。"""
    from datetime import datetime

    now = datetime.now()
    return f"当前时间是: {now.strftime('%Y年%m月%d日 %H:%M:%S')}"


@tool
def get_current_date() -> str:
    """获取当前日期工具。返回今天的日期。"""
    from datetime import datetime

    now = datetime.now()
    return f"今天是: {now.strftime('%Y年%m月%d日')}"


@tool
def fetch_webpage(url: str) -> str:
    """网页预览工具。获取指定URL的网页内容摘要。"""
    try:
        headers = {"User-Agent": "Mozilla/5.0"}
        response = requests.get(url, headers=headers, timeout=10, verify=False)
        response.encoding = "utf-8"

        import re

        content = response.text
        content = re.sub(
            r"<script[^>]*>.*?</script>", "", content, flags=re.DOTALL | re.IGNORECASE
        )
        content = re.sub(
            r"<style[^>]*>.*?</style>", "", content, flags=re.DOTALL | re.IGNORECASE
        )
        content = re.sub(r"<[^>]+>", "", content)
        content = re.sub(r"\s+", " ", content).strip()
        content = content[:2000]

        return f"网页标题: {response.url}\n\n内容预览:\n{content}..."
    except Exception as e:
        return f"获取网页失败: {str(e)}"


@tool
def translate_to_chinese(text: str) -> str:
    """翻译工具。将英文翻译成中文。"""
    try:
        url = "https://api.mymemory.translated.net/get"
        params = {"q": text, "langpair": "en|zh-CN"}
        response = requests.get(url, params=params, timeout=10)
        data = response.json()

        if data.get("responseStatus") == 200:
            return data.get("responseData", {}).get("translatedText", "")
        return f"翻译失败: {data.get('responseDetails', '未知错误')}"
    except Exception as e:
        return f"翻译请求失败: {str(e)}"


@tool
def read_file(file_path: str) -> str:
    """文件读取工具。读取本地文本文件内容。"""
    try:
        if not os.path.exists(file_path):
            return f"文件不存在: {file_path}"

        ext = os.path.splitext(file_path)[1].lower()
        allowed_exts = {
            ".txt",
            ".md",
            ".py",
            ".js",
            ".json",
            ".yaml",
            ".yml",
            ".xml",
            ".csv",
            ".log",
            ".html",
            ".css",
        }
        if ext not in allowed_exts:
            return f"不支持的文件类型: {ext}"

        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

        if len(content) > 5000:
            content = content[:5000] + f"\n\n... (共 {len(content)} 字符，已截断)"

        return f"文件: {file_path}\n\n{content}"
    except Exception as e:
        return f"读取文件失败: {str(e)}"


@tool
def generate_qrcode(text: str) -> str:
    """二维码生成工具。将指定文本或URL生成二维码，并在终端中显示。"""
    try:
        display_qrcode_in_kitty(text)
        return f"二维码已在终端显示\n内容: {text[:50]}{'...' if len(text) > 50 else ''}"
    except Exception as e:
        return f"生成二维码失败: {str(e)}"


@tool
def list_directory(path: str = ".") -> str:
    """目录列表工具。列出指定目录的内容。"""
    try:
        if not os.path.exists(path):
            return f"目录不存在: {path}"
        if not os.path.isdir(path):
            return f"不是目录: {path}"

        items = []
        for item in os.listdir(path):
            full_path = os.path.join(path, item)
            if os.path.isdir(full_path):
                items.append(f"📁 {item}/")
            else:
                size = os.path.getsize(full_path)
                items.append(f"📄 {item} ({size} bytes)")

        return f"目录 '{path}' 内容:\n\n" + "\n".join(items) if items else "目录为空"
    except Exception as e:
        return f"列出目录失败: {str(e)}"


def get_tool_list():
    """获取工具列表"""
    return [
        calculator,
        get_current_time,
        get_current_date,
        fetch_webpage,
        translate_to_chinese,
        read_file,
        list_directory,
        generate_qrcode,
    ]


# =============================================================================
# 第三部分: LangGraph 节点
# =============================================================================


def create_agent_node(model):
    """创建 Agent 节点函数"""

    def agent_node(state: AgentState) -> AgentState:
        messages = state["messages"]
        response = model.bind_tools(tools=get_tool_list()).invoke(messages)
        return {"messages": [response]}

    return agent_node


def create_tool_node():
    """创建工具执行节点"""

    def tool_node(state: AgentState) -> AgentState:
        last_message = state["messages"][-1]
        tool_calls = getattr(last_message, "tool_calls", None)
        if not tool_calls:
            return state

        tool_results = []
        for tool_call in tool_calls:
            tool_name = tool_call["name"]
            tool_args = tool_call.get("args", {})

            tool_func = None
            for t in get_tool_list():
                if t.name == tool_name:
                    tool_func = t
                    break

            if tool_func:
                try:
                    result = tool_func.invoke(tool_args)
                    tool_results.append(
                        ToolMessage(
                            content=str(result), tool_call_id=tool_call.get("id", "")
                        )
                    )
                except Exception as e:
                    tool_results.append(
                        ToolMessage(
                            content=f"工具执行错误: {str(e)}",
                            tool_call_id=tool_call.get("id", ""),
                        )
                    )
            else:
                tool_results.append(
                    ToolMessage(
                        content=f"未知工具: {tool_name}",
                        tool_call_id=tool_call.get("id", ""),
                    )
                )

        return {"messages": tool_results}

    return tool_node


# =============================================================================
# 第四部分: 构建 LangGraph 工作流
# =============================================================================


def create_agent_graph(model):
    """创建 Agent 工作流图"""
    workflow = StateGraph(AgentState)

    workflow.add_node("agent", create_agent_node(model))
    workflow.add_node("tools", create_tool_node())

    workflow.set_entry_point("agent")

    def should_continue(state: AgentState) -> str:
        messages = state["messages"]
        last_message = messages[-1]
        tool_calls = getattr(last_message, "tool_calls", None)
        return "tools" if tool_calls else "__end__"

    workflow.add_conditional_edges(
        "agent", should_continue, {"tools": "tools", "__end__": END}
    )

    workflow.add_edge("tools", "agent")

    return workflow.compile()


# =============================================================================
# 第五部分: 主程序入口
# =============================================================================


def main():
    """主函数"""
    print("=" * 60)
    print("本地 AI Agent - LangGraph + Ollama (Qwen3.5)")
    print("=" * 60)
    print()
    print("可用工具: calculator, get_current_time, get_current_date,")
    print("         fetch_webpage, translate_to_chinese,")
    print("         read_file, list_directory, generate_qrcode")
    print()
    print("输入 'quit' 或 'exit' 退出程序")
    print("-" * 60)

    model = ChatOllama(
        model="qwen3.5",
        temperature=0.7,
        keep_alive=-1,
    )

    system_message = """你是一个有用的 AI 助手。

可用工具:
- calculator: 数学计算
- get_current_time: 获取当前时间
- get_current_date: 获取当前日期
- fetch_webpage: 获取网页内容（需要URL）
- translate_to_chinese: 英文翻译成中文
- read_file: 读取本地文件
- list_directory: 列出目录内容
- generate_qrcode: 生成二维码（在终端中显示）"""

    graph = create_agent_graph(model)
    messages: list[BaseMessage] = [SystemMessage(content=system_message)]

    while True:
        try:
            user_input = input("\n你: ").strip()

            if user_input.lower() in ["quit", "exit", "q"]:
                print("\n再见！")
                break

            if not user_input:
                continue

            messages.append(HumanMessage(content=user_input))
            result = graph.invoke({"messages": messages})

            response = None
            for msg in reversed(result["messages"]):
                if isinstance(msg, AIMessage) and msg.content:
                    response = msg
                    break

            if response:
                print(f"\nAI: {response.content}")
            else:
                print(f"\nAI: (无回复)")

            # 只保留 SystemMessage + 最后一轮对话，避免消息历史过长
            # 过滤掉空的 AIMessage 和 ToolMessage
            new_messages = [messages[0]]  # SystemMessage
            for msg in result["messages"]:
                if isinstance(msg, HumanMessage):
                    new_messages.append(msg)
                elif isinstance(msg, AIMessage) and msg.content:
                    new_messages.append(msg)
            messages = new_messages

        except KeyboardInterrupt:
            print("\n\n程序被中断")
            break
        except Exception as e:
            print(f"\n错误: {str(e)}")


if __name__ == "__main__":
    main()
