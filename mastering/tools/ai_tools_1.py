import os
import subprocess
import gc
import re
import sys
from typing import Final, Any
from collections.abc import Iterator
from llama_cpp import Llama

# ================= 配置区 =================
VIDEO_DIR: Final = "/home/neo/Videos/ytb/tsoding/machine_learning_in_c"
WHISPER_MODEL_PATH: Final = "/home/neo/models/ggml-large-v3-turbo.bin"
LLM_MODEL_PATH: Final = "/home/neo/models/qwen2-7b-instruct.Q5_K_M.gguf"

# 7B 模型建议设置
BATCH_SIZE: Final = 6  # 保持适中，确保翻译质量
CONTEXT_WINDOW: Final = 8192  # 足够处理 BATCH 字幕
# ==========================================

def get_srt_paths(video_path: str) -> tuple[str, str]:
    base: str = video_path.rsplit('.', 1)[0]
    return f"{base}.en.srt", f"{base}.bilingual.srt"

def run_whisper_cpp(video_path: str, en_srt_path: str) -> bool:
    """提取英文字幕"""
    if os.path.exists(en_srt_path):
        print(f"⏩ 跳过识别: {os.path.basename(en_srt_path)}")
        return True

    temp_wav: str = f"{video_path.rsplit('.', 1)[0]}_temp.wav"
    output_base: str = en_srt_path.rsplit('.srt', 1)[0]

    try:
        print(f"🎵 [FFmpeg] 提取音频: {os.path.basename(video_path)}")
        subprocess.run([
            "ffmpeg", "-y", "-i", video_path, "-ar", "16000", "-ac", "1", 
            "-c:a", "pcm_s16le", "-loglevel", "quiet", temp_wav
        ], check=True)

        print(f"🎙️ [Whisper] 正在生成英文字幕...")
        subprocess.run([
            "whisper-cpp", "-m", WHISPER_MODEL_PATH, "-f", temp_wav, 
            "-osrt", "-of", output_base, "-l", "en"
        ], check=True, capture_output=True)

        return os.path.exists(en_srt_path)
    except subprocess.CalledProcessError as e:
        print(f"❌ 识别失败: {e}")
        return False
    finally:
        if os.path.exists(temp_wav):
            os.remove(temp_wav)

def chunk_list(lst: list[Any], n: int) -> Iterator[list[Any]]:
    for i in range(0, len(lst), n):
        yield lst[i : i + n]

def translate_batch(llm: Llama, batch: list[tuple[str, str, str]]) -> list[str]:
    """参考你提供的逻辑，使用 create_chat_completion 批量处理"""
    # 合并批次文本，带上行号 L0, L1... 引导模型按行翻译
    source_text = "\n".join([f"L{i}: {b[2]}" for i, b in enumerate(batch)])
    
    messages = [
        {
            "role": "user",
            "content": (
                "You are a professional subtitle translator for Tsoding's programming videos.\n"
                "Translate the following English lines into natural, concise Chinese.\n"
                "Keep variable names and technical terms as is.\n"
                "Maintain the line structure (one translation per line).\n"
                "Only output the Chinese translation.\n\n"
                f"{source_text}"
            ),
        }
    ]

    try:
        response = llm.create_chat_completion(
            messages=messages,
            max_tokens=1024,
            temperature=0.1,
            top_p=0.95,
            # 这里的 stop 标记可以根据需要微调，防止模型生成多余解释
            stop=["\n\n", "###", "English:"],
            repeat_penalty=1.1,
        )
        raw_output = response["choices"][0]["message"]["content"].strip()
        
        # 清理行号 L0:, L1: 标记
        lines = [re.sub(r'^L\d+[:：]\s*', '', line).strip() for line in raw_output.split('\n') if line.strip()]
        return lines
    except Exception as e:
        print(f"⚠️ Batch translation failed: {e}", file=sys.stderr)
        return []

def process_video_translation(en_srt_path: str, bi_srt_path: str, llm_path: str) -> None:
    """循环读取文件并翻译"""
    if os.path.exists(bi_srt_path):
        print(f"⏩ 跳过翻译: {os.path.basename(bi_srt_path)}")
        return

    print(f"🧠 [Llama] 加载 7B 模型 (VRAM 占用低，安全运行)...")
    llm = Llama(model_path=llm_path, n_gpu_layers=-1, n_ctx=CONTEXT_WINDOW, flash_attn=True, verbose=False)

    try:
        with open(en_srt_path, "r", encoding="utf-8") as f:
            content = f.read()
            blocks = re.findall(r"(\d+)\n(\d{2}:.*)\n(.*?)\n\n", content, re.DOTALL)

        print(f"✍️ [Batch] 正在翻译 {os.path.basename(en_srt_path)}...")
        with open(bi_srt_path, "w", encoding="utf-8") as f_out:
            for batch in chunk_list(blocks, BATCH_SIZE):
                translated_lines = translate_batch(llm, batch)
                
                for i, (idx, time, en_text) in enumerate(batch):
                    zh_text = translated_lines[i] if i < len(translated_lines) else "[翻译待重试]"
                    f_out.write(f"{idx}\n{time}\n{en_text}\n{zh_text}\n\n")
                f_out.flush()
    finally:
        del llm
        gc.collect()

def main() -> None:
    valid_exts = (".mp4", ".mkv", ".webm", ".ts")
    if not os.path.exists(VIDEO_DIR):
        print(f"❌ 目录不存在: {VIDEO_DIR}")
        return

    video_files = [os.path.join(VIDEO_DIR, f) for f in os.listdir(VIDEO_DIR) if f.lower().endswith(valid_exts)]
    
    for video in video_files:
        en_srt, bi_srt = get_srt_paths(video)
        if run_whisper_cpp(video, en_srt):
            process_video_translation(en_srt, bi_srt, LLM_MODEL_PATH)
            print(f"✨ 字幕完成: {os.path.basename(bi_srt)}")

if __name__ == "__main__":
    main()
