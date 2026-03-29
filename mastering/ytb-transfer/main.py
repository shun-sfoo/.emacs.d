"""
视频字幕生成工作流

自动完成: 下载播放列表视频 -> 按顺序重命名 -> 提取音频 -> Whisper 生成英文字幕 -> Ollama 翻译为中英双语字幕

工作流程:
    1. 下载: 使用 yt-dlp 下载 YouTube 播放列表视频
    2. 重命名: 按播放列表顺序重命名为 01-xxx.webm 格式
    3. 提取音频: 使用 ffmpeg 将视频转为 16kHz 单声道 WAV
    4. 生成字幕: 使用 whisper-cli 生成英文字幕 (_en.srt)
    5. 断句合并: 按时间间隔和标点将字幕合并成自然句子
    6. 翻译: 使用 Ollama 本地大模型翻译为中文
    7. 输出: 生成中英双语字幕 (.srt)

依赖:
    - Python 3.12+
    - ffmpeg (音频提取)
    - yt-dlp (视频下载)
    - whisper-cli (语音识别)
    - ollama (本地翻译服务)
    - srt (字幕解析库)

用法:
    uv run main.py                    # 完整流程
    uv run main.py --download-only    # 仅下载
    uv run main.py --sync-archive     # 仅同步下载记录
    uv run main.py video.webm         # 处理单个视频
"""

import argparse
import ast
import json
import re
import subprocess
import sys
import tomllib
import urllib.request
from dataclasses import dataclass
from datetime import timedelta
from pathlib import Path
from typing import Any

import srt


# ================================================================
#  数据模型
# ================================================================


@dataclass(frozen=True, slots=True)
class PlaylistEntry:
    """
    YouTube 播放列表条目

    属性:
        index: 视频在播放列表中的序号 (从 1 开始)
        id: YouTube 视频 ID (11 位字符)
        title: 视频标题
    """

    index: str
    id: str
    title: str


# ================================================================
#  配置
# ================================================================


class Config:
    """
    项目配置管理

    从 config.toml 读取配置，包括:
    - whisper: 语音识别相关配置 (CLI 路径、模型路径、识别语言)
    - ollama: 翻译相关配置 (API 地址、模型名称、批处理大小)
    - playlist: YouTube 播放列表地址 (可选)

    类属性:
        BASE_DIR: 项目根目录
        VIDEO_EXT: 支持的视频文件扩展名
        VIDEO_ID_RE: 匹配 YouTube 视频 ID 的正则表达式
        ARCHIVE_FILE: yt-dlp 下载记录文件路径
    """

    BASE_DIR = Path(__file__).parent
    VIDEO_EXT = {".mp4", ".mkv", ".webm", ".avi", ".mov"}
    VIDEO_ID_RE = re.compile(r"\[([a-zA-Z0-9_-]{11})\]$")
    ARCHIVE_FILE = BASE_DIR / ".yt-dlp-archive"

    def __init__(self) -> None:
        """从 config.toml 加载配置"""
        config = tomllib.loads((self.BASE_DIR / "config.toml").read_text())

        # Whisper 语音识别配置
        self.whisper_cli: str = config["whisper"]["cli"]
        self.whisper_model: str = config["whisper"]["model"]
        self.whisper_language: str = config["whisper"]["language"]

        # Ollama 翻译配置
        self.ollama_api: str = config["ollama"]["api"]
        self.ollama_model: str = config["ollama"]["model"]
        self.batch_size: int = int(config["ollama"]["batch_size"])

        # 播放列表地址 (可选)
        self.playlist_url: str = config.get("playlist", {}).get("url", "")


# ================================================================
#  播放列表管理
# ================================================================


class PlaylistManager:
    """
    YouTube 播放列表管理

    负责:
    - 获取播放列表中的视频信息
    - 将本地视频与播放列表匹配
    - 同步下载记录到 .yt-dlp-archive
    - 按播放列表顺序重命名视频文件
    """

    def __init__(self, config: Config) -> None:
        self._config = config

    @staticmethod
    def slugify(title: str) -> str:
        """
        将标题转换为 URL 友好的 slug 格式

        规则:
        - 转为小写
        - 移除特殊字符 (只保留字母、数字、空格、连字符)
        - 空格替换为连字符
        - 合并连续连字符

        Args:
            title: 原始标题

        Returns:
            slug 格式的字符串 (如 "music-visualizer")
        """
        s = title.lower()
        s = re.sub(r"[^a-z0-9\s-]", "", s)
        s = re.sub(r"[\s]+", "-", s)
        return re.sub(r"-+", "-", s).strip("-")

    @staticmethod
    def extract_video_id(name: str) -> str | None:
        """
        从文件名中提取 YouTube 视频 ID

        文件名格式: "Title [VIDEO_ID].ext"
        视频 ID 为 11 位字母数字字符

        Args:
            name: 文件名

        Returns:
            11 位视频 ID，如果未找到则返回 None
        """
        m = Config.VIDEO_ID_RE.search(Path(name).stem)
        return m.group(1) if m else None

    def fetch(self) -> list[PlaylistEntry]:
        """
        获取播放列表中的所有视频

        使用 yt-dlp 的 --flat-playlist 选项快速获取视频列表，
        不下载任何内容。

        Returns:
            播放列表条目列表，如果未配置播放列表则返回空列表
        """
        if not self._config.playlist_url:
            return []

        # 调用 yt-dlp 获取视频 ID 和标题
        r = subprocess.run(
            [
                "yt-dlp",
                "--print",
                "%(id)s|%(title)s",
                "--flat-playlist",
                self._config.playlist_url,
            ],
            capture_output=True,
            text=True,
            check=True,
        )

        # 解析输出: "VIDEO_ID|Video Title"
        entries: list[PlaylistEntry] = []
        for i, line in enumerate(r.stdout.strip().split("\n")):
            if "|" not in line:
                continue
            vid_id, title = line.split("|", 1)
            entries.append(
                PlaylistEntry(
                    index=str(i + 1),
                    id=vid_id.strip(),
                    title=title.strip(),
                )
            )
        return entries

    def match_local(self) -> list[tuple[PlaylistEntry, Path]]:
        """
        将本地视频文件与播放列表匹配

        通过视频 ID 匹配本地文件和播放列表条目。

        Returns:
            (播放列表条目, 本地文件路径) 的元组列表
        """
        entries = self.fetch()
        if not entries:
            return []

        # 构建 ID -> 条目 的映射
        id_map = {e.id: e for e in entries}

        # 遍历本地文件，查找匹配的视频 ID
        matched: dict[str, Path] = {}
        for f in Path(".").iterdir():
            if f.suffix.lower() in Config.VIDEO_EXT:
                vid_id = self.extract_video_id(f.name)
                if vid_id and vid_id in id_map:
                    matched[vid_id] = f

        return [(e, matched[e.id]) for e in entries if e.id in matched]

    def sync_archive(self) -> None:
        """
        同步下载记录到 .yt-dlp-archive

        将本地存在的视频 ID 写入存档文件，
        防止 yt-dlp 重复下载。
        """
        entries = self.fetch()
        if not entries:
            return

        # 读取已有的存档记录
        existing: set[str] = set()
        if Config.ARCHIVE_FILE.exists():
            existing = {
                line.strip().split()[-1]
                for line in Config.ARCHIVE_FILE.read_text().splitlines()
                if line.strip()
            }

        # 构建本地文件的 ID 映射
        local_files = list(Path(".").iterdir())
        id_to_file: dict[str, Path] = {}
        for f in local_files:
            if f.suffix.lower() not in Config.VIDEO_EXT:
                continue
            vid_id = self.extract_video_id(f.name)
            if vid_id:
                id_to_file[vid_id] = f

        # 通过 slug 匹配未找到 ID 的文件
        for entry in entries:
            if entry.id in id_to_file:
                continue
            slug = self.slugify(entry.title)
            for f in local_files:
                if f.suffix.lower() in Config.VIDEO_EXT and slug in f.stem:
                    id_to_file[entry.id] = f
                    break

        # 写入新的 ID 到存档
        new_ids = [e.id for e in entries if e.id in id_to_file and e.id not in existing]
        if new_ids:
            with open(Config.ARCHIVE_FILE, "a") as archive:
                for vid_id in new_ids:
                    archive.write(f"youtube {vid_id}\n")
            print(f"已同步 {len(new_ids)} 个视频ID到 {Config.ARCHIVE_FILE.name}")

    def rename_videos(self) -> list[Path]:
        """
        按播放列表顺序重命名视频文件

        命名格式: {序号}-{slug}.ext
        例如: 01-music-visualizer-fast-fourier-transform.webm

        同时重命名关联的音频和字幕文件 (.wav, _en.srt, .srt)

        Returns:
            重命名后的视频文件路径列表
        """
        pairs = self.match_local()
        if not pairs:
            return []

        # 读取已有的存档 ID
        existing_ids: set[str] = set()
        if Config.ARCHIVE_FILE.exists():
            existing_ids = {
                line.strip().split()[-1]
                for line in Config.ARCHIVE_FILE.read_text().splitlines()
                if line.strip()
            }

        result: list[Path] = []
        for entry, old_path in pairs:
            # 生成新文件名
            prefix = entry.index.zfill(2)  # 补零: "1" -> "01"
            slug = self.slugify(entry.title)
            new_stem = f"{prefix}-{slug}"
            new_name = f"{new_stem}{old_path.suffix}"

            # 记录到存档
            if entry.id not in existing_ids:
                with open(Config.ARCHIVE_FILE, "a") as archive:
                    archive.write(f"youtube {entry.id}\n")
                existing_ids.add(entry.id)

            # 文件名未变化，跳过
            if old_path.name == new_name:
                result.append(old_path)
                continue

            # 重命名关联文件 (.wav, _en.srt, .srt)
            new_path = old_path.with_name(new_name)
            for suffix in [".wav", "_en.srt", ".srt"]:
                old_r = old_path.with_name(old_path.stem + suffix)
                new_r = old_path.with_name(new_stem + suffix)
                if old_r.exists() and old_r != new_r:
                    old_r.rename(new_r)
                    print(f"  重命名: {old_r.name} -> {new_r.name}")

            # 重命名视频文件
            old_path.rename(new_path)
            print(f"  重命名: {old_path.name} -> {new_name}")
            result.append(new_path)

        return result


# ================================================================
#  下载器
# ================================================================


class Downloader:
    """
    视频下载器

    使用 yt-dlp 下载 YouTube 播放列表视频。
    通过 .yt-dlp-archive 文件避免重复下载。
    """

    def __init__(self, config: Config) -> None:
        self._config = config

    def download(self) -> None:
        """下载播放列表中的新视频"""
        if not self._config.playlist_url:
            return

        print(f"下载视频: {self._config.playlist_url}")
        subprocess.run(
            [
                "yt-dlp",
                "--download-archive",
                str(Config.ARCHIVE_FILE),
                "-o",
                "%(title)s [%(id)s].%(ext)s",
                self._config.playlist_url,
            ],
            check=True,
        )


# ================================================================
#  音频提取器
# ================================================================


class AudioExtractor:
    """
    音频提取器

    使用 ffmpeg 从视频中提取音频。
    输出格式: 16kHz 单声道 WAV (PCM 16-bit LE)
    这是 Whisper 语音识别要求的音频格式。
    """

    @staticmethod
    def extract(video: Path) -> Path:
        """
        从视频文件提取音频

        Args:
            video: 视频文件路径

        Returns:
            生成的 WAV 文件路径
        """
        wav = video.with_suffix(".wav")
        print(f"提取音频: {video.name} -> {wav.name}")

        subprocess.run(
            [
                "ffmpeg",
                "-y",  # 覆盖已存在的文件
                "-i",
                str(video),  # 输入文件
                "-ar",
                "16000",  # 采样率 16kHz
                "-ac",
                "1",  # 单声道
                "-c:a",
                "pcm_s16le",  # 编码: PCM 16-bit Little Endian
                "-stats",  # 显示进度
                str(wav),  # 输出文件
            ],
            check=True,
        )
        print(f"完成: {wav.name}")
        return wav


# ================================================================
#  字幕生成器 (Whisper)
# ================================================================


class SubtitleGenerator:
    """
    英文字幕生成器

    使用 whisper-cli (whisper.cpp) 从音频生成英文字幕。
    输出格式: SRT (SubRip Text)
    """

    def __init__(self, config: Config) -> None:
        self._config = config

    def generate(self, wav: Path) -> Path:
        """
        从音频文件生成英文字幕

        Args:
            wav: WAV 音频文件路径

        Returns:
            生成的 SRT 字幕文件路径 (xxx_en.srt)
        """
        srt_path = wav.with_name(wav.stem + "_en.srt")
        output_base = str(srt_path.with_suffix(""))
        print(f"生成字幕: {wav.name} -> {srt_path.name}")

        subprocess.run(
            [
                self._config.whisper_cli,
                "-m",
                self._config.whisper_model,
                "-f",
                str(wav),
                "--language",
                self._config.whisper_language,
                "--output-srt",
                "-of",
                output_base,
            ],
            check=True,
        )
        print(f"完成: {srt_path.name}")
        return srt_path


# ================================================================
#  翻译器 (Ollama)
# ================================================================


class Translator:
    """
    Ollama 翻译器

    使用本地 Ollama 大模型翻译英文字幕为中文。
    支持批量翻译和单条重试机制。

    特性:
    - 批量翻译提高效率
    - 自动检测并重试空翻译
    - 支持多种返回格式 (JSON/Python 列表/字典)
    - 自动过滤占位符 (如 "翻译1", "翻译2")
    """

    SYSTEM_PROMPT = "你是一位精通底层开发（C/C++）和图形学的资深翻译，请翻译 Tsoding 的编程直播字幕。"

    def __init__(self, config: Config) -> None:
        self._config = config

    def _parse_response(self, raw: str) -> list[str]:
        """
        解析 Ollama 返回的结果

        支持多种格式:
        - JSON 数组: ["翻译1", "翻译2"]
        - Python 列表: ['翻译1', '翻译2']
        - 嵌套数组: [["翻译1", "翻译2"]]
        - 字典: {"1": "翻译1", "2": "翻译2"}
        - 带元数据的字典: [{"translated": "翻译1", "status": "..."}]

        Args:
            raw: Ollama 返回的原始字符串

        Returns:
            解析后的翻译列表，解析失败返回空列表
        """
        # 尝试 JSON 解析
        try:
            result: Any = json.loads(raw)
        except json.JSONDecodeError:
            # 尝试 Python 字面量解析 (处理 '...' 格式)
            try:
                result = ast.literal_eval(raw)
            except (ValueError, SyntaxError):
                return []

        # 处理列表格式
        if isinstance(result, list):
            # 展开嵌套列表 [["a", "b"]] -> ["a", "b"]
            if result and isinstance(result[0], list):
                result = result[0]

            cleaned: list[str] = []
            for t in result:
                # 处理带元数据的字典 {"translated": "翻译"}
                if isinstance(t, dict):
                    t = t.get("translated", t.get("text", ""))
                t = str(t)
                # 过滤占位符 "翻译1", "翻译2" 等
                if re.match(r"^翻译\d+$", t.strip()):
                    cleaned.append("")
                else:
                    cleaned.append(t)
            return cleaned

        # 处理字典格式 {"1": "翻译1", "2": "翻译2"}
        if isinstance(result, dict):
            sorted_items = sorted(
                result.items(),
                key=lambda x: int(m.group()) if (m := re.search(r"\d+", x[0])) else 0,
            )
            return [str(v) for _, v in sorted_items]

        return []

    def _call_ollama(self, prompt: str, timeout: int = 300) -> str:
        """
        调用 Ollama API

        Args:
            prompt: 提示词
            timeout: 超时时间 (秒)

        Returns:
            模型返回的文本
        """
        payload = json.dumps(
            {
                "model": self._config.ollama_model,
                "prompt": prompt,
                "stream": False,  # 不使用流式输出
                "think": False,  # 不输出思考过程
            }
        ).encode("utf-8")

        req = urllib.request.Request(
            self._config.ollama_api,
            data=payload,
            headers={"Content-Type": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            data = json.loads(resp.read().decode("utf-8"))
        return data.get("response", "").strip()

    def translate_batch(self, texts: list[str]) -> list[str]:
        """
        批量翻译文本

        流程:
        1. 发送批量翻译请求
        2. 解析返回结果
        3. 检测空翻译并逐条重试

        Args:
            texts: 待翻译的英文文本列表

        Returns:
            中文翻译列表，与输入一一对应
        """
        if not texts:
            return []

        # 构建批量翻译 prompt
        numbered = "\n".join(f"{i + 1}. {t}" for i, t in enumerate(texts))
        prompt = (
            f"{self.SYSTEM_PROMPT}\n\n"
            "请将以下字幕翻译成中文，每条翻译对应一行，用 JSON 数组返回：\n\n"
            + numbered
        )

        # 调用 Ollama
        try:
            raw = self._call_ollama(prompt)
            translations = self._parse_response(raw)
        except Exception as e:
            print(f"  ollama翻译出错: {e}")
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
                    prompt = f"{self.SYSTEM_PROMPT}\n\n请将以下字幕翻译成中文，只输出翻译：\n{texts[idx]}"
                    raw = self._call_ollama(prompt, timeout=60)
                    parsed = self._parse_response(raw)
                    translations[idx] = parsed[0] if parsed else raw
                except Exception:
                    pass

        return translations[: len(texts)]


# ================================================================
#  字幕处理器
# ================================================================


class SubtitleProcessor:
    """
    字幕处理器

    负责:
    - 解析 SRT 字幕文件
    - 按时间间隔和标点合并断句
    - 生成双语字幕文件

    断句合并规则:
    - 时间间隔超过 500ms 视为新句子
    - 句末标点后断句
    - 超过 200 字符强制断句
    """

    # 断句参数
    GAP_THRESHOLD = timedelta(milliseconds=500)  # 时间间隔阈值
    SENTENCE_END = re.compile(r'[.!?。！？]"?\s*$')  # 句末标点
    MAX_LENGTH = 200  # 单句最大长度

    @staticmethod
    def parse(path: Path) -> list[srt.Subtitle]:
        """
        解析 SRT 字幕文件

        Args:
            path: SRT 文件路径

        Returns:
            字幕对象列表
        """
        content = path.read_text(encoding="utf-8")
        return list(srt.parse(content))

    @classmethod
    def merge_sentences(cls, subs: list[srt.Subtitle]) -> list[srt.Subtitle]:
        """
        按时间间隔和标点将字幕合并成自然句子

        将 Whisper 生成的碎片化字幕合并成更自然的句子，
        提高翻译质量和上下文连贯性。

        Args:
            subs: 原始字幕列表

        Returns:
            合并后的字幕列表
        """
        if not subs:
            return []

        merged: list[srt.Subtitle] = []
        current_start = subs[0].start
        current_end = subs[0].end
        current_text = subs[0].content.replace("\n", " ").strip()

        for sub in subs[1:]:
            text = sub.content.replace("\n", " ").strip()
            gap = sub.start - current_end

            # 判断是否应该断句
            should_break = (
                gap > cls.GAP_THRESHOLD  # 时间间隔大
                or cls.SENTENCE_END.search(current_text)  # 句末标点
                or len(current_text) + len(text) + 1 > cls.MAX_LENGTH  # 超过最大长度
            )

            if should_break:
                # 保存当前句子
                merged.append(
                    srt.Subtitle(
                        index=len(merged) + 1,
                        start=current_start,
                        end=current_end,
                        content=current_text,
                    )
                )
                # 开始新句子
                current_start = sub.start
                current_text = text
            else:
                # 合并到当前句子
                current_text = current_text.rstrip() + " " + text

            current_end = sub.end

        # 保存最后一条
        merged.append(
            srt.Subtitle(
                index=len(merged) + 1,
                start=current_start,
                end=current_end,
                content=current_text,
            )
        )

        # 重新编号
        for i, sub in enumerate(merged, 1):
            sub.index = i

        return merged

    @staticmethod
    def compose(
        subs: list[srt.Subtitle],
        translations: list[str],
        output_path: Path,
    ) -> None:
        """
        生成双语字幕文件

        每条字幕格式:
            英文原文
            中文翻译

        Args:
            subs: 英文字幕列表
            translations: 中文翻译列表
            output_path: 输出文件路径
        """
        output_subs: list[srt.Subtitle] = []
        for sub, cn in zip(subs, translations):
            output_subs.append(
                srt.Subtitle(
                    index=sub.index,
                    start=sub.start,
                    end=sub.end,
                    content=f"{sub.content}\n{cn}",
                )
            )
        output_path.write_text(srt.compose(output_subs), encoding="utf-8")


# ================================================================
#  工作流
# ================================================================


class Workflow:
    """
    视频字幕生成工作流

    协调各个组件完成完整的字幕生成流程:
    1. 音频提取
    2. 英文字幕生成
    3. 断句合并
    4. 中文翻译
    5. 双语字幕输出

    支持断点续传: 已存在的文件会自动跳过。
    """

    def __init__(self) -> None:
        """初始化工作流组件"""
        self.config = Config()
        self.playlist = PlaylistManager(self.config)
        self.downloader = Downloader(self.config)
        self.audio = AudioExtractor()
        self.subtitle_gen = SubtitleGenerator(self.config)
        self.translator = Translator(self.config)
        self.processor = SubtitleProcessor()

    def process_video(self, video: Path) -> None:
        """
        处理单个视频文件

        流程:
        1. 提取音频 (xxx.wav)
        2. 生成英文字幕 (xxx_en.srt)
        3. 翻译并生成双语字幕 (xxx.srt)

        每步检查输出文件是否存在，存在则跳过。

        Args:
            video: 视频文件路径
        """
        stem = video.stem
        wav = video.with_suffix(".wav")
        en_srt = video.with_name(stem + "_en.srt")
        srt_path = video.with_suffix(".srt")

        print(f"\n{'=' * 60}")
        print(f"处理: {video.name}")
        print(f"{'=' * 60}")

        # 步骤 1: 提取音频
        if wav.exists():
            print(f"跳过音频提取: {wav.name}")
        else:
            self.audio.extract(video)

        # 步骤 2: 生成英文字幕
        if en_srt.exists():
            print(f"跳过字幕生成: {en_srt.name}")
        else:
            self.subtitle_gen.generate(wav)

        # 步骤 3: 翻译生成双语字幕
        if srt_path.exists():
            print(f"跳过翻译: {srt_path.name}")
        else:
            self._translate(en_srt)

        print(f"完成! 双语字幕: {srt_path.name}")

    def _translate(self, en_srt: Path) -> Path:
        """
        翻译英文字幕为中英双语

        流程:
        1. 解析英文字幕
        2. 断句合并 (提高翻译质量)
        3. 批量翻译
        4. 生成双语字幕文件

        Args:
            en_srt: 英文字幕文件路径

        Returns:
            生成的双语字幕文件路径
        """
        output_srt = en_srt.with_name(en_srt.stem.removesuffix("_en") + ".srt")

        # 1. 解析字幕
        subs = self.processor.parse(en_srt)
        print(f"原始字幕: {len(subs)} 条")

        # 2. 断句合并
        merged = self.processor.merge_sentences(subs)
        print(f"合并后: {len(merged)} 条")

        # 3. 翻译
        texts = [sub.content for sub in merged]
        total = len(texts)
        batch_size = self.config.batch_size
        print(f"开始翻译（每批{batch_size}条）...")

        all_cn: list[str] = []
        for i in range(0, total, batch_size):
            batch = texts[i : i + batch_size]
            end = min(i + batch_size, total)
            print(f"  翻译 {i + 1}-{end}/{total}...")
            cn_list = self.translator.translate_batch(batch)

            # 补齐缺失的翻译
            while len(cn_list) < len(batch):
                cn_list.append("")
            cn_list = [cn if cn else batch[j] for j, cn in enumerate(cn_list)]

            # 打印翻译结果
            for text, cn in zip(batch, cn_list):
                print(f"    {text[:70]}")
                print(f"    {cn[:70]}\n")

            all_cn.extend(cn_list)

        # 4. 生成双语字幕
        self.processor.compose(merged, all_cn, output_srt)
        print(f"完成: {output_srt.name}")
        return output_srt


# ================================================================
#  入口
# ================================================================


def main() -> None:
    """
    程序入口

    解析命令行参数，执行相应的工作流。
    """
    # 解析命令行参数
    p = argparse.ArgumentParser(description="视频字幕生成工作流")
    p.add_argument("video", nargs="?", help="指定视频文件")
    p.add_argument("--download-only", action="store_true", help="仅下载")
    p.add_argument("--sync-archive", action="store_true", help="仅同步下载记录")
    args = p.parse_args()

    # 创建工作流实例
    wf = Workflow()

    # 仅同步存档
    if args.sync_archive:
        wf.playlist.sync_archive()
        return

    # 确定要处理的视频列表
    if args.video:
        videos = [Path(args.video)]
    else:
        wf.playlist.sync_archive()
        wf.downloader.download()
        videos = wf.playlist.rename_videos() or sorted(
            f for f in Path(".").iterdir() if f.suffix.lower() in Config.VIDEO_EXT
        )

    # 仅下载模式
    if args.download_only:
        return

    # 检查视频列表
    if not videos:
        print("未找到视频文件")
        sys.exit(1)

    print(f"找到 {len(videos)} 个视频文件:")
    for v in videos:
        print(f"  - {v.name}")

    # 处理每个视频
    for video in videos:
        if video.with_suffix(".srt").exists():
            print(f"\n跳过（已翻译）: {video.name}")
            continue
        wf.process_video(video)


if __name__ == "__main__":
    main()
