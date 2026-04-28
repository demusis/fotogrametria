"""
Utilitários FFmpeg/FFprobe para extração e navegação de vídeo forense.

Encapsula a comunicação com os binários FFmpeg/FFprobe via subprocess,
fornecendo uma interface Python para:
- Obtenção de metadados (resolução, FPS, duração, total de frames)
- Extração exata de timestamps (PTS) via FFprobe
- Leitura sequencial de frames como arrays numpy RGB float64 [0, 1]
"""

import subprocess
import json
import numpy as np


class FFmpegPlayer:
    """Leitor de vídeo baseado em FFmpeg com suporte a seek preciso.

    Atributos
    ---------
    path         : str   — caminho do arquivo de vídeo
    width        : int   — largura em pixels
    height       : int   — altura em pixels
    fps          : float — taxa de quadros por segundo
    duration     : float — duração total em segundos
    total_frames : int   — número total de quadros
    pts_list     : list[float] — timestamps exatos (PTS) de cada quadro
    """

    def __init__(self, path: str):
        self.path = path
        self.process: subprocess.Popen | None = None
        self.width = 0
        self.height = 0
        self.fps = 30.0
        self.duration = 0.0
        self.total_frames = 0
        self.frame_size = 0
        self.pts_list: list[float] = []
        self._load_metadata()

    def _load_metadata(self):
        """Carrega metadados do vídeo via FFprobe (formato JSON)."""
        cmd = [
            "ffprobe", "-v", "quiet", "-print_format", "json",
            "-show_format", "-show_streams", self.path
        ]
        result = subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", errors="replace")
        if result.returncode != 0:
            raise RuntimeError("Erro ao ler metadata com ffprobe.")

        info = json.loads(result.stdout)
        video_stream = next(
            (s for s in info.get("streams", []) if s.get("codec_type") == "video"),
            None,
        )
        if not video_stream:
            raise RuntimeError("Nenhum stream de vídeo encontrado.")

        self.width = int(video_stream.get("width", 0))
        self.height = int(video_stream.get("height", 0))

        # FPS
        fps_frac = video_stream.get("r_frame_rate", "30/1")
        try:
            num, den = map(int, fps_frac.split("/"))
            self.fps = num / den if den != 0 else 30.0
        except (ValueError, ZeroDivisionError):
            self.fps = 30.0

        # Duração
        try:
            self.duration = float(
                video_stream.get("duration", info.get("format", {}).get("duration", 0))
            )
        except (ValueError, TypeError):
            self.duration = 0.0

        # Total de frames
        nb_frames = video_stream.get("nb_frames")
        if nb_frames:
            self.total_frames = int(nb_frames)
        else:
            self.total_frames = int(self.duration * self.fps)

        self.frame_size = self.width * self.height * 3

        # Extração exata de timestamps (PTS)
        self._load_pts()

    def _load_pts(self):
        """Extrai a lista de PTS (Presentation Timestamps) via FFprobe."""
        try:
            cmd_pts = [
                "ffprobe", "-v", "error", "-select_streams", "v:0",
                "-show_entries", "frame=pkt_pts_time", "-of", "csv=p=0",
                self.path,
            ]
            res = subprocess.run(cmd_pts, capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=15)
            if res.returncode == 0:
                self.pts_list = [
                    float(x.strip())
                    for x in res.stdout.strip().split("\n")
                    if x.strip() and x.strip() != "N/A"
                ]
                if len(self.pts_list) > self.total_frames:
                    self.total_frames = len(self.pts_list)
        except (subprocess.TimeoutExpired, ValueError):
            pass

    def get_time_for_frame(self, frame_index: int) -> float:
        """Retorna o timestamp exato (PTS) para um dado índice de quadro.

        Se a lista de PTS estiver disponível, usa-a; caso contrário,
        calcula por interpolação linear (frame_index / fps).
        """
        if self.pts_list and 0 <= frame_index < len(self.pts_list):
            return self.pts_list[frame_index]
        return frame_index / self.fps if self.fps > 0 else 0.0

    def start_stream(self, start_time_sec: float = 0.0):
        """Inicia um stream de decodificação a partir do tempo especificado.

        Usa seeking rápido (flag -ss antes de -i) para posicionar no tempo
        desejado e decodifica frames como rawvideo RGB24.
        """
        self.close()
        cmd = [
            "ffmpeg", "-y",
            "-ss", str(max(0, start_time_sec)),
            "-i", self.path,
            "-f", "image2pipe",
            "-pix_fmt", "rgb24",
            "-vcodec", "rawvideo",
            "-an", "-sn", "-",
        ]
        self.process = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, bufsize=10**8
        )

    def read_next_frame(self) -> np.ndarray | None:
        """Lê o próximo frame do stream como array numpy RGB float64 [0, 1].

        Retorna None se não houver mais frames disponíveis.
        """
        if not self.process or not self.process.stdout:
            return None

        raw_bytes = self.process.stdout.read(self.frame_size)
        if len(raw_bytes) != self.frame_size:
            return None

        arr = np.frombuffer(raw_bytes, dtype=np.uint8).reshape(
            (self.height, self.width, 3)
        )
        return arr.astype(np.float64) / 255.0

    def close(self):
        """Encerra o processo FFmpeg se estiver em execução."""
        if self.process:
            try:
                self.process.kill()
            except OSError:
                pass
            self.process = None
