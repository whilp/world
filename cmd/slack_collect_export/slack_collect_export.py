#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import datetime
import json
import os
import string
import sys
import typing as t

from pathlib import Path

ARGS = argparse.ArgumentParser()
ARGS.add_argument("users")
ARGS.add_argument("channels")
ARGS.add_argument("files", nargs="+")


def main(argv: t.List[str], env: t.Dict[str, str]) -> None:
    args = ARGS.parse_args(argv[1:])
    mapping: t.Dict[str, str] = dict()
    for arg in [args.users, args.channels]:
        mapping.update(process_mapping(arg))

    writer = csv.DictWriter(sys.stdout, Message.fields)
    writer.writeheader()

    for name in args.files:
        for raw in process_file(name):
            channel = str(Path(name).parent)
            message = Message.init(channel, mapping, raw)
            message.writerow(writer)


Mapping = t.Dict[str, str]


class Text(string.Template):
    delimiter = "<"
    pattern = r"""
        <(?:
        (?P<escaped>\<\<)|
        [@#](?P<named>[A-Z0-9]*)\>|
        [@#](?P<braced>[A-Z0-9]*)\>|
        (?P<invalid>)
        )
    """


class Message(t.NamedTuple):
    channel: str
    user: str
    text: str
    timestamp: str
    fields = ["timestamp", "channel", "user", "text"]

    @classmethod
    def init(cls, channel: str, mapping: Mapping, message: t.Dict[str, t.Any]) -> Message:
        user = message.get("user", "")
        text = Text(message.get("text", "")).safe_substitute(mapping)

        return cls(
            channel=channel, text=text, timestamp=to_timestamp(message.get("ts", "")), user=mapping.get(user, user),
        )

    def writerow(self, writer: csv.DictWriter):
        row = dict([(k, v) for k, v in self._asdict().items() if k in self.fields])
        writer.writerow(row)


def to_timestamp(ts: str):
    return datetime.datetime.utcfromtimestamp(float(ts)).strftime("%Y-%m-%d %H:%M:%S")


def process_mapping(path: str) -> t.Dict[str, str]:
    return dict([(i["id"], i["name"]) for i in process_file(path)])


def process_file(name: str) -> t.Generator[t.Dict[t.Any, t.Any], None, None]:
    with open(name) as f:
        items = json.load(f)
        for item in items:
            yield item


if __name__ == "__main__":
    sys.exit(main(list(sys.argv), dict(os.environ)))
