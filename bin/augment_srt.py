#!/usr/bin/env python3
import sys
import os
import urllib.request
import json
import re
import time
from urllib.error import HTTPError


def process_batch(api_key, batch, language, level, max_retries=4):
    system_prompt = f"""You are a helpful language teacher. The user is watching a movie in {language} and their proficiency level is {level}.
You will be provided with a JSON array of subtitle objects, each containing an 'id' and 'text'.
Identify any words that might be difficult for someone at this level.
For each difficult word, provide its English translation.
If there are no difficult words in a subtitle, return an empty array for that id.
Return ONLY the JSON object matching the required schema."""

    # Define the JSON schema to strictly enforce the output structure
    schema = {
        "type": "object",
        "properties": {
            "results": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "id": {"type": "string"},
                        "difficult_words": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "word": {
                                        "type": "string",
                                        "description": f"The difficult word in {language} exactly as it appears in the text",
                                    },
                                    "translation": {
                                        "type": "string",
                                        "description": "The English translation of the word",
                                    },
                                },
                                "required": ["word", "translation"],
                                "additionalProperties": False,
                            },
                        },
                    },
                    "required": ["id", "difficult_words"],
                    "additionalProperties": False,
                },
            }
        },
        "required": ["results"],
        "additionalProperties": False,
    }

    # Groq provides an OpenAI-compatible endpoint
    url = "https://api.groq.com/openai/v1/chat/completions"
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    }

    # Using Llama 3.3 70B Versatile for robust JSON Schema support
    data = {
        "model": "moonshotai/kimi-k2-instruct-0905",
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": json.dumps(batch)},
        ],
        "response_format": {
            "type": "json_schema",
            "json_schema": {
                "name": "difficult_words_extraction",
                "schema": schema,
                "strict": True,
            },
        },
        "temperature": 0.0,  # Deterministic output
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
                return parsed.get("results", [])
        except HTTPError as e:
            error_body = e.read().decode("utf-8", errors="replace")
            if e.code == 429:  # Rate limit hit
                wait_time = (2**attempt) * 2
                print(
                    f"\nRate limit hit (429). Retrying in {wait_time} seconds...",
                    file=sys.stderr,
                )
                time.sleep(wait_time)
            else:
                print(f"\nHTTP Error {e.code}: {e.reason}", file=sys.stderr)
                print(f"Error Body: {error_body}", file=sys.stderr)
                time.sleep(2)
        except Exception as e:
            print(f"\nError: {type(e).__name__}: {str(e)}", file=sys.stderr)
            time.sleep(2)

    return None  # Return None if all retries fail


def apply_translations(original_text, difficult_words):
    text = original_text

    # We replace words one by one locally to ensure the original text structure is preserved.
    for item in difficult_words:
        word = item.get("word", "")
        translation = item.get("translation", "")
        if word and translation:
            # Escape the word for regex to safely match it
            escaped_word = re.escape(word)

            # Use word boundaries (\b) and ignore case (?i) to find the word
            pattern = r"(?i)\b" + escaped_word + r"\b"

            # Using a lambda to preserve the original casing of the matched word found in the text
            # E.g., if it matches "Apfel", it replaces with "Apfel [apple]"
            text = re.sub(pattern, lambda m: f"{m.group(0)} [{translation}]", text)

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
            parsed_blocks.append((seq, timing, text))
        else:
            parsed_blocks.append((None, None, block))

    subtitle_items = [
        {"id": b[0], "text": b[2]} for b in parsed_blocks if b[0] is not None
    ]
    total_items = len(subtitle_items)
    print(
        f"Processing {total_items} subtitle blocks using Groq API with JSON Schema..."
    )

    batch_size = 20
    augmented_texts = {}

    # Process sequentially in batches to respect Groq rate limits
    total_batches = (total_items + batch_size - 1) // batch_size
    for i in range(0, total_items, batch_size):
        batch = subtitle_items[i : i + batch_size]
        print(
            f"Processing batch {i // batch_size + 1}/{total_batches} (Items {i + 1} to {min(i + batch_size, total_items)})...",
            end="\r",
        )

        results = process_batch(api_key, batch, language, level)

        if results is None:
            failed_ids = [item["id"] for item in batch]
            print(
                f"\nFailed to process batch. Giving up on subtitle IDs: {', '.join(failed_ids)}",
                file=sys.stderr,
            )
            results = []

        # Map results to a dictionary keyed by the subtitle ID
        result_map = {
            str(r.get("id")): r.get("difficult_words", [])
            for r in results
            if isinstance(r, dict)
        }

        for item in batch:
            original_text = item["text"]
            seq_id = item["id"]
            diff_words = result_map.get(str(seq_id), [])

            # Deterministically apply substitutions in Python
            augmented_texts[seq_id] = apply_translations(original_text, diff_words)

        # Groq free tier allows 30 requests per minute (1 request per 2 seconds).
        # We wait 2.5 seconds here to guarantee we stay comfortably under the limit.
        time.sleep(2.5)

    print("\nProcessing complete!")

    new_blocks = []
    for b in parsed_blocks:
        if b[0] is not None:
            new_text = augmented_texts.get(b[0], b[2])
            new_blocks.append(f"{b[0]}\n{b[1]}\n{new_text}")
        else:
            new_blocks.append(b[2])

    with open(output_file, "w", encoding="utf-8") as f:
        f.write("\n\n".join(new_blocks) + "\n")
    print(f"Saved augmented subtitles to {output_file}")


if __name__ == "__main__":
    if len(sys.argv) < 5:
        print(
            "Usage: python augment_srt.py <input.srt> <output.srt> <language> <proficiency_level>"
        )
        print("Example: python augment_srt.py movie.srt movie_augmented.srt German B1")
        sys.exit(1)

    api_key = os.environ.get("GROQ_API_KEY")
    if not api_key:
        print("Error: GROQ_API_KEY environment variable is not set.", file=sys.stderr)
        print(
            "Please set it using: export GROQ_API_KEY='your_api_key_here'",
            file=sys.stderr,
        )
        sys.exit(1)

    input_srt = sys.argv[1]
    output_srt = sys.argv[2]
    lang = sys.argv[3]
    lvl = sys.argv[4]

    process_srt(input_srt, output_srt, lang, lvl, api_key)
