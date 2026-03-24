#!/usr/bin/env python3
import sys
import os
import urllib.request
import json
import re
import time
from urllib.error import HTTPError


def process_subtitle(api_key, subtitle, language, level, max_retries=4):
    system_prompt = f"""You are a helpful language teacher. The user is watching a movie in {language} and their proficiency level is {level}.
You will be provided with a single subtitle text.
Identify any words that might be difficult for someone at this level.
For each difficult word, provide its base form (including definite article for nouns, or infinitive for verbs) and its English translation.
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
                        "word": {
                            "type": "string",
                            "description": f"The difficult word in {language} exactly as it appears in the text",
                        },
                        "base_form": {
                            "type": "string",
                            "description": "The dictionary base form. For nouns, include the definite article (der/die/das). For verbs, use the infinitive.",
                        },
                        "translation": {
                            "type": "string",
                            "description": "The English translation of the word in this context",
                        },
                    },
                    "required": ["word", "base_form", "translation"],
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
                print(f"\nHTTP Error {e.code}: {e.reason}", file=sys.stderr)
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

            # Format: Original [der Baseform: translation]
            annotation = f"{base_form}: {translation}" if base_form else translation
            text = re.sub(pattern, lambda m: f"{m.group(0)} [{annotation}]", text)

    return text


def process_srt(input_file, output_file, language, level, api_key):
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

    for i, item in enumerate(subtitle_items):
        print(f"Processing item {i + 1}/{total_items}...", end="\r")

        difficult_words = process_subtitle(api_key, item, language, level)

        if difficult_words is None:
            print(f"\nFailed to process subtitle ID: {item['id']}", file=sys.stderr)
            continue

        item["text"] = apply_translations(item["text"], difficult_words)

        # Rate limit protection for sequential processing
        time.sleep(2.5)

    print("\nProcessing complete!")

    new_blocks = []
    for b in parsed_blocks:
        if "id" in b:
            new_blocks.append(f"{b['id']}\n{b['timing']}\n{b['text']}")
        else:
            new_blocks.append(b["raw"])

    with open(output_file, "w", encoding="utf-8") as f:
        f.write("\n\n".join(new_blocks) + "\n")
    print(f"Saved augmented subtitles to {output_file}")


if __name__ == "__main__":
    if len(sys.argv) < 5:
        print(
            "Usage: python augment_srt.py <input.srt> <output.srt> <language> <proficiency_level>"
        )
        sys.exit(1)

    api_key = os.environ.get("GROQ_API_KEY")
    if not api_key:
        print("Error: GROQ_API_KEY environment variable is not set.", file=sys.stderr)
        sys.exit(1)

    process_srt(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], api_key)
