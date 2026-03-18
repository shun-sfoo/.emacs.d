import os
import subprocess
import gc
import re
from typing import List, Dict, Any, Optional

# ================= 配置区 =================
VIDEO_DIR: str = "/home/neo/Videos/ytb/tsoding/machine_learning_in_c"
# 你的单文件 GGML 模型路径
WHISPER_MODEL_PATH: str = "/home/neo/models/ggml-large-v3-turbo.bin" 
LLM_MODEL_PATH: str = "/home/neo/models/qwen2.5-coder-14b-instruct-q6_k.gguf"

# Tsoding 专属翻译提示词
TSODING_SYSTEM_PROMPT: str = (
    "你是一位精通底层开发(C/C++、Zig)和机器学习的资深架构师。 "
    "现在请为 Tsoding 的编程视频翻译字幕。 "
    "要求：1. 术语专业（如 Gradient Descent 译为梯度下降）。 "
    "2. 保留变量名和特定代码术语。 "
    "3. 英文在上面，中文在下面。只输出翻译结果。"
)
# ==========================================

def run_whisper_cpp(video_path: str) -> Optional[str]:
    """
    1. 使用 ffmpeg 提取 16kHz 单声道 WAV (whisper.cpp 必需)
    2. 调用 whisper-cpp 生成英文 srt
    """
    base_path: str = video_path.rsplit('.', 1)[0]
    wav_path: str = f"{base_path}_temp.wav"
    output_srt: str = f"{base_path}.en" # whisper-cpp 会自动加 .srt 扩展名

    try:
        # Step 1: 提取音频
        print(f"🎵 正在提取音频: {os.path.basename(video_path)}")
        subprocess.run([
            "ffmpeg", "-y", "-i", video_path, 
            "-ar", "16000", "-ac", "1", "-c:a", "pcm_s16le", wav_path
        ], check=True, capture_output=True)

        # Step 2: 运行 whisper-cpp (开启 CUDA 加速)
        print(f"🎙️ 正在识别 (whisper.cpp): {os.path.basename(video_path)}")
        # -osrt 输出字幕, -l en 强制英文, -nt 不打印进度
        subprocess.run([
            "whisper-cli", 
            "-m", WHISPER_MODEL_PATH, 
            "-f", wav_path, 
            "-osrt", 
            "-of", output_srt,
            "-l", "en"
        ], check=True)

        return f"{output_srt}.srt"
    except subprocess.CalledProcessError as e:
        print(f"❌ 处理失败: {e}")
        return None
    finally:
        if os.path.exists(wav_path):
            os.remove(wav_path)

def translate_srt_file(en_srt_path: str, llm_path: str) -> None:
    """读取英文 SRT，利用 llama-cpp-python 翻译成双语版本"""
    from llama_cpp import Llama # 延迟加载以防显存过早占用

    bi_srt_path: str = en_srt_path.replace(".en.srt", ".bilingual.srt")
    
    print(f"🧠 加载翻译模型: {os.path.basename(llm_path)}")
    llm = Llama(model_path=llm_path, n_gpu_layers=-1, n_ctx=2048, verbose=False)

    with open(en_srt_path, "r", encoding="utf-8") as f_in, \
         open(bi_srt_path, "w", encoding="utf-8") as f_out:
        
        content: str = f_in.read()
        # 匹配 SRT 块: 序号, 时间轴, 文本
        blocks = re.findall(r"(\d+)\n(\d{2}:.*)\n(.*?)\n\n", content, re.DOTALL)

        for index, timeline, text in blocks:
            clean_text: str = text.replace("\n", " ")
            prompt: str = f"{TSODING_SYSTEM_PROMPT}\n\nEnglish: {clean_text}\nChinese:"
            
            output = llm(prompt, max_tokens=128, stop=["\n", "English:"], temperature=0.1)
            zh_text: str = output['choices'][0]['text'].strip()

            # 英文在上，中文在下
            f_out.write(f"{index}\n{timeline}\n{clean_text}\n{zh_text}\n\n")
            f_out.flush()
    
    # 释放显存给下一个视频或代码补全
    del llm
    gc.collect()

def main() -> None:
    valid_exts = (".mp4", ".mkv", ".webm", ".ts")
    videos = [os.path.join(VIDEO_DIR, f) for f in os.listdir(VIDEO_DIR) if f.endswith(valid_exts)]
    
    if not videos:
        print("未发现视频。")
        return

    print(f"🚀 开始处理 {len(videos)} 个 Tsoding 视频...")

    for video in videos:
        # 1. 识别
        en_srt = run_whisper_cpp(video)
        if en_srt and os.path.exists(en_srt):
            # 2. 翻译
            translate_srt_file(en_srt, LLM_MODEL_PATH)
            # 可选：删除原始仅英文的 srt
            # os.remove(en_srt)

if __name__ == "__main__":
    main()
