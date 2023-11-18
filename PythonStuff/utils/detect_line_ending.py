from pathlib import Path


def detect_line_ending(file_path: Path) -> str:
    endings = {"\r\n": 0, "\r": 0, "\n": 0}

    with file_path.open(mode="r", encoding="utf-8") as fd:
        for line in fd:
            if line.endswith("\r\n"):
                endings["\r\n"] += 1

            elif line.endswith("\r"):
                endings["\r"] += 1

            elif line.endswith("\n"):
                endings["\n"] += 1

    return max(endings, key=endings.get)
