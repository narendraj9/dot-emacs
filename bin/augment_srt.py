#!/usr/bin/env python3
import sys
import os
import urllib.request
import json
import re
import time
import csv
from urllib.error import HTTPError


def process_subtitle(api_key, subtitle, language, level, max_retries=4):
    system_prompt = f"""You are a helpful language teacher. The user is watching a movie in {language} and their proficiency level is {level}.
You will be provided with a single subtitle text.
Identify any words that might be difficult for someone at this level.
For each difficult word, provide:
1. Its dictionary base form.
2. Its English translation.
3. Exactly two simple example sentences in {language} using the word.
If there are no difficult words, return an empty array.

Return ONLY the JSON object matching the required schema."""

    schema = {
        "type": "object",
        "properties": {
            "difficult_words": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "word": {"type": "string"},
                        "base_form": {"type": "string"},
                        "translation": {"type": "string"},
                        "example_1": {
                            "type": "string",
                            "description": "First example sentence",
                        },
                        "example_2": {
                            "type": "string",
                            "description": "Second example sentence",
                        },
                    },
                    "required": [
                        "word",
                        "base_form",
                        "translation",
                    ],
                    "additionalProperties": False,
                },
            }
        },
        "required": ["difficult_words"],
        "additionalProperties": False,
    }

    url = "https://api.groq.com/openai/v1/chat/completions"
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}",
        "User-Agent": "Mozilla/5.0",
    }

    data = {
        "model": "moonshotai/kimi-k2-instruct-0905",
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": subtitle["text"]},
        ],
        "response_format": {
            "type": "json_schema",
            "json_schema": {
                "name": "difficult_words_extraction",
                "schema": schema,
                "strict": True,
            },
        },
        "temperature": 0.0,
    }

    req = urllib.request.Request(
        url, headers=headers, data=json.dumps(data).encode("utf-8")
    )

    for attempt in range(max_retries):
        try:
            with urllib.request.urlopen(req) as response:
                res_data = json.loads(response.read().decode())
                content = res_data["choices"][0]["message"]["content"]
                parsed = json.loads(content)
                return parsed.get("difficult_words", [])
        except HTTPError as e:
            if e.code == 429:
                wait_time = (2**attempt) * 2
                print(f"\nRate limit hit. Retrying in {wait_time}s...", file=sys.stderr)
                time.sleep(wait_time)
            else:
                error_body = e.read().decode("utf-8")
                print(
                    f"\nHTTP Error {e.code}: {e.reason}, {error_body}", file=sys.stderr
                )
                time.sleep(2)
        except Exception as e:
            print(f"\nError: {type(e).__name__}: {str(e)}", file=sys.stderr)
            time.sleep(2)

    return None


def apply_translations(original_text, difficult_words):
    text = original_text
    for item in difficult_words:
        word = item.get("word", "")
        base_form = item.get("base_form", "")
        translation = item.get("translation", "")

        if word and translation:
            escaped_word = re.escape(word)
            pattern = r"(?i)\b" + escaped_word + r"\b"
            annotation = f"{base_form}: {translation}" if base_form else translation
            text = re.sub(pattern, lambda m: f"{m.group(0)} [{annotation}]", text)

    return text


def export_to_anki(all_words, output_file):
    unique_words = {}
    for word_data in all_words:
        base = word_data.get("base_form")
        if base and base not in unique_words:
            unique_words[base] = word_data

    # Using tab delimiter as Anki parses TSV files reliably without quote escaping issues
    with open(output_file, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, delimiter="\t")
        for w in unique_words.values():
            writer.writerow(
                [
                    w.get("base_form", ""),
                    w.get("translation", ""),
                    w.get("example_1", ""),
                    w.get("example_2", ""),
                ]
            )
    print(f"Exported {len(unique_words)} unique words to {output_file} for Anki.")


def process_srt(input_file, output_srt, output_anki, language, level, api_key):
    with open(input_file, "r", encoding="utf-8") as f:
        content = f.read().replace("\r\n", "\n")

    blocks = re.split(r"\n\n+", content.strip())
    parsed_blocks = []

    for block in blocks:
        lines = block.split("\n")
        if len(lines) >= 3:
            seq = lines[0]
            timing = lines[1]
            text = "\n".join(lines[2:])
            parsed_blocks.append({"id": seq, "timing": timing, "text": text})
        else:
            parsed_blocks.append({"raw": block})

    subtitle_items = [b for b in parsed_blocks if "id" in b]
    total_items = len(subtitle_items)
    print(f"Processing {total_items} subtitle blocks sequentially...")

    global_difficult_words = []

    for i, item in enumerate(subtitle_items):
        print(f"Processing item {i + 1}/{total_items}...", end="\r")

        difficult_words = process_subtitle(api_key, item, language, level)

        if difficult_words is None:
            print(f"\nFailed to process subtitle ID: {item['id']}", file=sys.stderr)
            continue

        global_difficult_words.extend(difficult_words)
        item["text"] = apply_translations(item["text"], difficult_words)

    print("\nProcessing complete!")

    new_blocks = []
    for b in parsed_blocks:
        if "id" in b:
            new_blocks.append(f"{b['id']}\n{b['timing']}\n{b['text']}")
        else:
            new_blocks.append(b["raw"])

    with open(output_srt, "w", encoding="utf-8") as f:
        f.write("\n\n".join(new_blocks) + "\n")
    print(f"Saved augmented subtitles to {output_srt}")

    export_to_anki(global_difficult_words, output_anki)


if __name__ == "__main__":
    if len(sys.argv) < 6:
        print(
            "Usage: python augment_srt.py <input.srt> <output.srt> <anki_output.tsv> <language> <proficiency_level>"
        )
        sys.exit(1)

    api_key = os.environ.get("GROQ_API_KEY")
    if not api_key:
        print("Error: GROQ_API_KEY environment variable is not set.", file=sys.stderr)
        sys.exit(1)

    process_srt(
        sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5], api_key
    )
