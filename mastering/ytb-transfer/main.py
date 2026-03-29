"""
视频字幕生成工作流
  下载 -> 重命名 -> 提取音频 -> 英文字幕 -> 断句合并 -> 中英双语字幕
"""

import argparse
import json
import re
import subprocess
import sys
import tomllib
import urllib.request
from datetime import timedelta
from pathlib import Path

import srt

# ================================================================
#  配置
# ================================================================

BASE_DIR = Path(__file__).parent
CONFIG = tomllib.loads((BASE_DIR / "config.toml").read_text())
WHISPER = CONFIG["whisper"]
OLLAMA = CONFIG["ollama"]
PLAYLIST_URL = CONFIG.get("playlist", {}).get("url", "")

VIDEO_EXT = {".mp4", ".mkv", ".webm", ".avi", ".mov"}
VIDEO_ID_RE = re.compile(r"\[([a-zA-Z0-9_-]{11})\]$")
ARCHIVE_FILE = BASE_DIR / ".yt-dlp-archive"

# ================================================================
#  播放列表
# ================================================================


def slugify(title: str) -> str:
    s = title.lower()
    s = re.sub(r"[^a-z0-9\s-]", "", s)
    s = re.sub(r"[\s]+", "-", s)
    return re.sub(r"-+", "-", s).strip("-")


def fetch_playlist() -> list[dict[str, str]]:
    if not PLAYLIST_URL:
        return []
    r = subprocess.run(
        ["yt-dlp", "--print", "%(id)s|%(title)s", "--flat-playlist", PLAYLIST_URL],
        capture_output=True,
        text=True,
        check=True,
    )
    entries = []
    for i, line in enumerate(r.stdout.strip().split("\n")):
        if "|" not in line:
            continue
        vid_id, title = line.split("|", 1)
        entries.append(
            {"index": str(i + 1), "id": vid_id.strip(), "title": title.strip()}
        )
    return entries


def extract_video_id(name: str) -> str | None:
    m = VIDEO_ID_RE.search(Path(name).stem)
    return m.group(1) if m else None


def match_playlist() -> list[tuple[dict[str, str], Path]]:
    entries = fetch_playlist()
    if not entries:
        return []
    id_map = {e["id"]: e for e in entries}
    matched: dict[str, Path] = {}
    for f in Path(".").iterdir():
        if f.suffix.lower() in VIDEO_EXT:
            vid_id = extract_video_id(f.name)
            if vid_id and vid_id in id_map:
                matched[vid_id] = f
    return [(e, matched[e["id"]]) for e in entries if e["id"] in matched]


def sync_archive():
    entries = fetch_playlist()
    if not entries:
        return
    existing: set[str] = set()
    if ARCHIVE_FILE.exists():
        existing = {
            l.strip().split()[-1]
            for l in ARCHIVE_FILE.read_text().splitlines()
            if l.strip()
        }

    local_files = list(Path(".").iterdir())
    id_to_file: dict[str, Path] = {}
    for f in local_files:
        if f.suffix.lower() not in VIDEO_EXT:
            continue
        vid_id = extract_video_id(f.name)
        if vid_id:
            id_to_file[vid_id] = f
    for entry in entries:
        if entry["id"] in id_to_file:
            continue
        slug = slugify(entry["title"])
        for f in local_files:
            if f.suffix.lower() in VIDEO_EXT and slug in f.stem:
                id_to_file[entry["id"]] = f
                break

    new_ids = [
        e["id"] for e in entries if e["id"] in id_to_file and e["id"] not in existing
    ]
    if new_ids:
        with open(ARCHIVE_FILE, "a") as f:
            for vid_id in new_ids:
                f.write(f"youtube {vid_id}\n")
        print(f"已同步 {len(new_ids)} 个视频ID到 {ARCHIVE_FILE.name}")


def rename_videos() -> list[Path]:
    pairs = match_playlist()
    if not pairs:
        return []
    existing_ids: set[str] = set()
    if ARCHIVE_FILE.exists():
        existing_ids = {
            l.strip().split()[-1]
            for l in ARCHIVE_FILE.read_text().splitlines()
            if l.strip()
        }

    result: list[Path] = []
    for entry, old_path in pairs:
        prefix = entry["index"].zfill(2)
        slug = slugify(entry["title"])
        new_stem = f"{prefix}-{slug}"
        new_name = f"{new_stem}{old_path.suffix}"

        if entry["id"] not in existing_ids:
            with open(ARCHIVE_FILE, "a") as f:
                f.write(f"youtube {entry['id']}\n")
            existing_ids.add(entry["id"])

        if old_path.name == new_name:
            result.append(old_path)
            continue

        new_path = old_path.with_name(new_name)
        for suffix in [".wav", "_en.srt", ".srt"]:
            old_r = old_path.with_name(old_path.stem + suffix)
            new_r = old_path.with_name(new_stem + suffix)
            if old_r.exists() and old_r != new_r:
                old_r.rename(new_r)
                print(f"  重命名: {old_r.name} -> {new_r.name}")
        old_path.rename(new_path)
        print(f"  重命名: {old_path.name} -> {new_name}")
        result.append(new_path)
    return result


# ================================================================
#  下载
# ================================================================


def download_videos():
    if not PLAYLIST_URL:
        return
    print(f"下载视频: {PLAYLIST_URL}")
    subprocess.run(
        [
            "yt-dlp",
            "--download-archive",
            str(ARCHIVE_FILE),
            "-o",
            "%(title)s [%(id)s].%(ext)s",
            PLAYLIST_URL,
        ],
        check=True,
    )


# ================================================================
#  提取音频
# ================================================================


def extract_audio(video: Path) -> Path:
    wav = video.with_suffix(".wav")
    print(f"提取音频: {video.name} -> {wav.name}")
    subprocess.run(
        [
            "ffmpeg",
            "-y",
            "-i",
            str(video),
            "-ar",
            "16000",
            "-ac",
            "1",
            "-c:a",
            "pcm_s16le",
            "-stats",
            str(wav),
        ],
        check=True,
    )
    print(f"完成: {wav.name}")
    return wav


# ================================================================
#  生成英文字幕 (whisper)
# ================================================================


def generate_subtitles(wav: Path) -> Path:
    srt = wav.with_name(wav.stem + "_en.srt")
    output_base = str(srt.with_suffix(""))
    print(f"生成字幕: {wav.name} -> {srt.name}")
    subprocess.run(
        [
            str(WHISPER["cli"]),
            "-m",
            str(WHISPER["model"]),
            "-f",
            str(wav),
            "--language",
            str(WHISPER["language"]),
            "--output-srt",
            "-of",
            output_base,
        ],
        check=True,
    )
    print(f"完成: {srt.name}")
    return srt


# ================================================================
#  翻译为中英双语字幕 (ollama)
# ================================================================

SYSTEM_PROMPT = (
    "你是一位精通底层开发（C/C++）和图形学的资深翻译，请翻译 Tsoding 的编程直播字幕。"
)


def parse_srt(path: Path) -> list[dict[str, str]]:
    content = path.read_text(encoding="utf-8")
    segments = []
    for block in re.split(r"\n\n+", content.strip()):
        lines = block.strip().split("\n")
        if len(lines) < 3:
            continue
        segments.append(
            {
                "index": lines[0].strip(),
                "timestamp": lines[1].strip(),
                "text": "\n".join(lines[2:]).strip(),
            }
        )
    return segments


def translate_batch(texts: list[str]) -> list[str]:
    if not texts:
        return []

    numbered = "\n".join(f"{i + 1}. {t}" for i, t in enumerate(texts))
    prompt = (
        f"{SYSTEM_PROMPT}\n\n"
        "请将以下字幕翻译成中文，每条翻译对应一行，用 JSON 数组返回：\n\n" + numbered
    )
    payload = json.dumps(
        {
            "model": OLLAMA["model"],
            "prompt": prompt,
            "stream": False,
            "think": False,
        }
    ).encode("utf-8")

    def _call() -> list[str]:
        import ast

        req = urllib.request.Request(
            str(OLLAMA["api"]),
            data=payload,
            headers={"Content-Type": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=300) as resp:
            data = json.loads(resp.read().decode("utf-8"))
        raw = data.get("response", "").strip()
        # 尝试解析 JSON 或 Python 列表
        try:
            result = json.loads(raw)
        except json.JSONDecodeError:
            try:
                result = ast.literal_eval(raw)
            except (ValueError, SyntaxError):
                result = []

        if isinstance(result, list):
            # 展开嵌套列表
            if result and isinstance(result[0], list):
                result = result[0]
            # 检测占位符和字典格式
            cleaned = []
            for t in result:
                if isinstance(t, dict):
                    t = t.get("translated", t.get("text", ""))
                t = str(t)
                if re.match(r"^翻译\d+$", t.strip()):
                    cleaned.append("")
                else:
                    cleaned.append(t)
            return cleaned
        if isinstance(result, dict):
            sorted_items = sorted(
                result.items(),
                key=lambda x: int(m.group()) if (m := re.search(r"\d+", x[0])) else 0,
            )
            return [str(v) for _, v in sorted_items]
        return []

    try:
        translations = _call()
    except Exception as e:
        print(f"  ollama翻译出错: {e}")
        # 返回空列表触发补翻
        translations = []

    # 补齐缺失的条目
    while len(translations) < len(texts):
        translations.append("")

    # 重试空翻译
    empty = [i for i, t in enumerate(translations) if not t.strip()]
    if empty:
        print(f"  补翻 {len(empty)} 条...")
        for idx in empty:
            try:
                single_payload = json.dumps(
                    {
                        "model": OLLAMA["model"],
                        "prompt": f"{SYSTEM_PROMPT}\n\n请将以下字幕翻译成中文，只输出翻译：\n{texts[idx]}",
                        "stream": False,
                        "think": False,
                    }
                ).encode("utf-8")
                req = urllib.request.Request(
                    str(OLLAMA["api"]),
                    data=single_payload,
                    headers={"Content-Type": "application/json"},
                )
                with urllib.request.urlopen(req, timeout=60) as resp:
                    data = json.loads(resp.read().decode("utf-8"))
                raw = data.get("response", "").strip()
                # 尝试解析 JSON 或 Python 列表
                try:
                    # 先尝试 JSON
                    parsed = json.loads(raw)
                except json.JSONDecodeError:
                    # 再尝试 Python ast.literal_eval
                    import ast

                    try:
                        parsed = ast.literal_eval(raw)
                    except (ValueError, SyntaxError):
                        parsed = raw

                if isinstance(parsed, list) and parsed:
                    translations[idx] = str(parsed[0])
                else:
                    translations[idx] = str(parsed)
            except Exception:
                pass

    return translations[: len(texts)]


# 断句合并参数
GAP_THRESHOLD = timedelta(milliseconds=500)
SENTENCE_END = re.compile(r'[.!?。！？]"?\s*$')
MAX_LENGTH = 200


def merge_sentences(subs: list[srt.Subtitle]) -> list[srt.Subtitle]:
    """按时间间隔和标点将字幕合并成自然句子"""
    if not subs:
        return []

    merged = []
    current_start = subs[0].start
    current_end = subs[0].end
    current_text = subs[0].content.replace("\n", " ").strip()

    for sub in subs[1:]:
        text = sub.content.replace("\n", " ").strip()
        gap = sub.start - current_end

        should_break = (
            gap > GAP_THRESHOLD
            or SENTENCE_END.search(current_text)
            or len(current_text) + len(text) + 1 > MAX_LENGTH
        )

        if should_break:
            merged.append(
                srt.Subtitle(
                    index=len(merged) + 1,
                    start=current_start,
                    end=current_end,
                    content=current_text,
                )
            )
            current_start = sub.start
            current_text = text
        else:
            current_text = current_text.rstrip() + " " + text

        current_end = sub.end

    merged.append(
        srt.Subtitle(
            index=len(merged) + 1,
            start=current_start,
            end=current_end,
            content=current_text,
        )
    )

    for i, sub in enumerate(merged, 1):
        sub.index = i

    return merged


def timedelta_to_ass(td) -> str:
    """将 timedelta 转换为 ASS 时间格式 H:MM:SS.CC"""
    total_seconds = int(td.total_seconds())
    hours = total_seconds // 3600
    minutes = (total_seconds % 3600) // 60
    seconds = total_seconds % 60
    centiseconds = int(td.microseconds / 10000)
    return f"{hours}:{minutes:02d}:{seconds:02d}.{centiseconds:02d}"


def generate_ass(subs: list[srt.Subtitle], translations: list[str], output_path: Path):
    """生成高级 ASS 字幕：英文白色，中文黄色半透明小字"""
    header = """[Script Info]
; Script generated by Music Visualizer
ScriptType: v4.00+
PlayResX: 1920
PlayResY: 1080
WrapStyle: 0

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Arial,48,&H00FFFFFF,&H000000FF,&H00000000,&H80000000,-1,0,0,0,100,100,0,0,1,2,0,2,10,10,200,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
"""
    lines = [header]
    for sub, cn in zip(subs, translations):
        start = timedelta_to_ass(sub.start)
        end = timedelta_to_ass(sub.end)
        # 英文白色，换行，中文黄色半透明小字
        text = f"{sub.content}\\N{{\\alpha&H80&\\c&H00D0FF&\\fs32}}{cn}"
        lines.append(f"Dialogue: 0,{start},{end},Default,,0,0,0,,{text}\n")

    output_path.write_text("".join(lines), encoding="utf-8")


def translate_srt(en_srt: Path) -> Path:
    output_srt = en_srt.with_name(en_srt.stem.removesuffix("_en") + ".srt")
    batch_size = int(OLLAMA["batch_size"])

    # 1. 解析字幕
    content = en_srt.read_text(encoding="utf-8")
    subs = list(srt.parse(content))
    print(f"原始字幕: {len(subs)} 条")

    # 2. 断句合并
    merged = merge_sentences(subs)
    print(f"合并后: {len(merged)} 条")

    # 3. 翻译
    texts = [sub.content for sub in merged]
    total = len(texts)
    print(f"开始翻译（每批{batch_size}条）...")

    all_cn: list[str] = []
    for i in range(0, total, batch_size):
        batch = texts[i : i + batch_size]
        end = min(i + batch_size, total)
        print(f"  翻译 {i + 1}-{end}/{total}...")
        cn_list = translate_batch(batch)
        # 补齐缺失和空的翻译
        while len(cn_list) < len(batch):
            cn_list.append("")
        cn_list = [cn if cn else batch[j] for j, cn in enumerate(cn_list)]
        for text, cn in zip(batch, cn_list):
            print(f"    {text[:70]}")
            print(f"    {cn[:70]}\n")
        all_cn.extend(cn_list)

    # 4. 生成双语字幕
    output_subs = []
    for sub, cn in zip(merged, all_cn):
        output_subs.append(
            srt.Subtitle(
                index=sub.index,
                start=sub.start,
                end=sub.end,
                content=f"{sub.content}\n{cn}",
            )
        )

    output_srt.write_text(srt.compose(output_subs), encoding="utf-8")
    print(f"完成: {output_srt.name}")
    return output_srt


# ================================================================
#  主流程
# ================================================================


def process_video(video: Path):
    stem = video.stem
    wav = video.with_suffix(".wav")
    en_srt = video.with_name(stem + "_en.srt")
    srt = video.with_suffix(".srt")

    print(f"\n{'=' * 60}")
    print(f"处理: {video.name}")
    print(f"{'=' * 60}")

    if wav.exists():
        print(f"跳过音频提取: {wav.name}")
    else:
        extract_audio(video)

    if en_srt.exists():
        print(f"跳过字幕生成: {en_srt.name}")
    else:
        generate_subtitles(wav)

    if srt.exists():
        print(f"跳过翻译: {srt.name}")
    else:
        translate_srt(en_srt)

    print(f"完成! 双语字幕: {srt.name}")


def main():
    p = argparse.ArgumentParser(description="视频字幕生成工作流")
    p.add_argument("video", nargs="?", help="指定视频文件")
    p.add_argument("--download-only", action="store_true", help="仅下载")
    p.add_argument("--sync-archive", action="store_true", help="仅同步下载记录")
    args = p.parse_args()

    if args.sync_archive:
        sync_archive()
        return

    if args.video:
        videos = [Path(args.video)]
    else:
        sync_archive()
        download_videos()
        videos = rename_videos() or sorted(
            f for f in Path(".").iterdir() if f.suffix.lower() in VIDEO_EXT
        )

    if args.download_only:
        return

    if not videos:
        print("未找到视频文件")
        sys.exit(1)

    print(f"找到 {len(videos)} 个视频文件:")
    for v in videos:
        print(f"  - {v.name}")

    for video in videos:
        if video.with_suffix(".srt").exists():
            print(f"\n跳过（已翻译）: {video.name}")
            continue
        process_video(video)


if __name__ == "__main__":
    main()
