"""
LangGraph Agent 使用示例
"""

from agent import create_agent_graph, ChatOllama, SystemMessage, HumanMessage
from langchain_core.messages import AIMessage, BaseMessage


def main():
    print("=" * 60)
    print("LangGraph Agent 使用示例")
    print("=" * 60)
    
    model = ChatOllama(model="qwen3.5:0.8b", temperature=0.7)
    graph = create_agent_graph(model)
    
    system_prompt = """你是一个有帮助的AI助手。

你可以:
- 回答各种问题
- 进行数学计算 (使用 calculator 工具)
- 告诉你当前时间 (使用 get_current_time 工具)
- 告诉你今天日期 (使用 get_current_date 工具)"""
    
    messages: list[BaseMessage] = [SystemMessage(content=system_prompt)]
    
    print("\n可以尝试以下问题:")
    print("  - '你好'")
    print("  - '帮我计算 123 * 456'")
    print("  - '现在几点钟?'")
    print("  - '今天是几号?'")
    print()
    print("输入 'quit' 退出")
    print("-" * 60)
    
    while True:
        user_input = input("\n你: ").strip()
        
        if user_input.lower() in ["quit", "exit", "q"]:
            print("再见!")
            break
            
        if not user_input:
            continue
        
        messages.append(HumanMessage(content=user_input))
        
        result = graph.invoke({"messages": messages})
        
        for msg in reversed(result["messages"]):
            if isinstance(msg, AIMessage):
                print(f"\nAI: {msg.content}")
                break
        
        messages = result["messages"]


if __name__ == "__main__":
    main()
