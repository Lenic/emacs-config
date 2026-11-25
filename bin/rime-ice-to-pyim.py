#!/usr/bin/env python3
"""把雾凇拼音（https://github.com/iDvel/rime-ice）的词库转换成 pyim 词库。

用法：
  bin/rime-ice-to-pyim.py                 从 GitHub 下载词库并生成 pyim/rime-ice.pyim
  bin/rime-ice-to-pyim.py --src DIR       使用 DIR 下已有的 *.dict.yaml，不再下载
  bin/rime-ice-to-pyim.py --dicts 8105 41448 base ext
                                          自定义要合并的词库（默认 8105 base ext）

rime 词库每行是「词<TAB>空格分隔的拼音<TAB>权重」，pyim 词库每行是
「用 - 连接的拼音 词1 词2 ...」。同一拼音下的词按权重从高到低排列，pyim
就按这个顺序给出候选。没有拼音列的词库（如 tencent）会被跳过。

生成的词库是 GPL-3.0 的雾凇拼音数据，只在本地使用，不提交到仓库。
"""

import argparse
import os
import re
import sys
import tempfile
import urllib.request
from collections import defaultdict

BASE_URL = "https://raw.githubusercontent.com/iDvel/rime-ice/main/cn_dicts/{}.dict.yaml"
DEFAULT_DICTS = ["8105", "base", "ext"]
DEFAULT_OUT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                           "pyim", "rime-ice.pyim")
CODE_RE = re.compile(r"^[a-z]+( [a-z]+)*$")


def read_rime_dict(path, table):
    """把 PATH 中的词条按 code -> {word: weight} 合并进 TABLE，返回读入条数。"""
    count = 0
    in_body = False
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\n")
            if not in_body:
                # YAML 头以单独一行 ... 结束
                in_body = line.strip() == "..."
                continue
            if not line or line.startswith("#"):
                continue
            parts = line.split("\t")
            if len(parts) < 2 or not CODE_RE.match(parts[1]):
                continue
            word, code = parts[0], parts[1].replace(" ", "-")
            if not word or " " in word:
                continue
            weight = int(parts[2]) if len(parts) > 2 and parts[2].isdigit() else 0
            words = table[code]
            if weight >= words.get(word, -1):
                words[word] = weight
            count += 1
    return count


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--src", help="已下载的 rime-ice cn_dicts 目录")
    parser.add_argument("--dicts", nargs="+", default=DEFAULT_DICTS,
                        help="要合并的词库名（默认：%(default)s）")
    parser.add_argument("-o", "--output", default=DEFAULT_OUT, help="输出文件（默认：%(default)s）")
    args = parser.parse_args()

    table = defaultdict(dict)
    with tempfile.TemporaryDirectory() as tmp:
        for name in args.dicts:
            if args.src:
                path = os.path.join(args.src, f"{name}.dict.yaml")
            else:
                path = os.path.join(tmp, f"{name}.dict.yaml")
                print(f"下载 {name}.dict.yaml ...", file=sys.stderr)
                urllib.request.urlretrieve(BASE_URL.format(name), path)
            n = read_rime_dict(path, table)
            print(f"{name}: {n} 条", file=sys.stderr)

    tmp_out = args.output + ".tmp"
    with open(tmp_out, "w", encoding="utf-8") as out:
        out.write(";; -*- coding: utf-8-unix; -*-\n")
        out.write(f";; 由 bin/rime-ice-to-pyim.py 从雾凇拼音生成，词库：{' '.join(args.dicts)}\n")
        for code in sorted(table):
            words = sorted(table[code].items(), key=lambda kv: -kv[1])
            out.write(code + " " + " ".join(w for w, _ in words) + "\n")
    os.replace(tmp_out, args.output)
    print(f"已生成 {args.output}：{len(table)} 个编码", file=sys.stderr)


if __name__ == "__main__":
    main()
