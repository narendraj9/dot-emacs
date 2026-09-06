#!/usr/bin/env bash
# Capture the audio currently playing on Linux speakers, transcribe it, and
# optionally ask an LLM for vocabulary/phrase help.
#
# Speech-to-text backends:
#   STT=deepgram  - sends each audio chunk to Deepgram prerecorded API
#   STT=groq      - sends each audio chunk to Groq audio transcriptions API
#   STT=whisper   - local Whisper via whisper.cpp `whisper-cli` or Python `whisper`
#   STT=cmd       - custom command in TRANSCRIBE_CMD, use {audio} as placeholder
#
# LLM backends:
#   LLM=none      - only print transcript
#   LLM=ollama    - local explanations via `ollama run $OLLAMA_MODEL`
#   LLM=groq      - Groq chat completions API
#   LLM=openai    - OpenAI-compatible chat completions API

set -Eeuo pipefail

usage() {
  cat <<'USAGE'
Usage:
  speaker-vocab.sh [start|once|sources|help]

Examples:
  # Deepgram transcription, print transcript only
  export DEEPGRAM_API_KEY=...
  STT=deepgram LLM=none ./bin/speaker-vocab.sh start

  # Deepgram transcription + local vocab explanations with Ollama
  export DEEPGRAM_API_KEY=...
  STT=deepgram LLM=ollama OLLAMA_MODEL=llama3.1 ./bin/speaker-vocab.sh start

  # Deepgram transcription + Groq vocab explanations
  export DEEPGRAM_API_KEY=...
  export GROQ_API_KEY=...
  STT=deepgram LLM=groq GROQ_MODEL=llama-3.3-70b-versatile ./bin/speaker-vocab.sh start

  # Groq transcription + Groq vocab explanations
  export GROQ_API_KEY=...
  STT=groq GROQ_STT_MODEL=whisper-large-v3-turbo LLM=groq ./bin/speaker-vocab.sh start

  # Fully local transcription with whisper.cpp + local Ollama explanations
  # Install/build whisper.cpp and download a ggml model first.
  STT=whisper WHISPER_MODEL=$HOME/models/ggml-small.en.bin LLM=ollama ./bin/speaker-vocab.sh start

  # Use Python OpenAI Whisper CLI locally, if installed as `whisper`
  STT=whisper WHISPER_MODEL_NAME=small.en LLM=none ./bin/speaker-vocab.sh once

  # Custom local transcriber; command must print transcript to stdout
  STT=cmd TRANSCRIBE_CMD='my-transcriber --text {audio}' ./bin/speaker-vocab.sh once

Commands:
  start    Continuously record speaker audio and process completed chunks. Default.
  once     Capture/transcribe one chunk, useful for testing.
  sources  List PulseAudio/PipeWire sources and monitor sources.
  help     Show this help.

Environment:
  AUDIO_SOURCE       Pulse/PipeWire source. Default: "$(pactl get-default-sink).monitor"
  CHUNK_SECONDS      Seconds per audio chunk. Default: 5
  POLL_SECONDS       How often to look for completed chunks. Default: 1
  LANGUAGE           Speech language code. Default: en
  TARGET_LANGUAGE    Language being learned. Default: derived from LANGUAGE
  LEARNER_LEVEL      Learner/exam level for vocabulary selection. Default: B1
  KNOWN_WORDS_FILE   Optional newline-delimited list of words to skip
  DISPLAY_MODE       normal | watch. Default: normal
  LATEST_FILE        Optional file to atomically update with latest compact help
  WATCH_MAX_COMPLETED_CHUNKS
                      In watch mode, drop stale completed chunks beyond this. Default: 1
  STT                deepgram | groq | whisper | cmd. Default: deepgram
  LLM                none | ollama | groq | openai. Default: none

Deepgram STT:
  DEEPGRAM_API_KEY   Required for STT=deepgram
  DG_MODEL           Deepgram model. Default: nova-3

Groq STT:
  GROQ_API_KEY       Required for STT=groq, also used by LLM=groq
  GROQ_BASE_URL      Default: https://api.groq.com/openai/v1
  GROQ_STT_MODEL     Default: whisper-large-v3-turbo
  GROQ_STT_PROMPT    Optional prompt/context sent to Groq transcription

Local Whisper STT:
  WHISPER_MODEL      whisper.cpp ggml model path. Default: ~/models/ggml-base.en.bin
  WHISPER_MODEL_NAME Python whisper model name. Default: base.en

LLM explanations:
  OLLAMA_MODEL       Default: llama3.1
  GROQ_API_KEY       Required for LLM=groq
  GROQ_BASE_URL      Default: https://api.groq.com/openai/v1
  GROQ_MODEL         Default: llama-3.3-70b-versatile
  OPENAI_API_KEY     Required for LLM=openai
  OPENAI_BASE_URL    Default: https://api.openai.com/v1
  OPENAI_MODEL       Default: gpt-4o-mini
USAGE
}

log() { printf '[%(%H:%M:%S)T] %s\n' -1 "$*" >&2; }
need() { command -v "$1" >/dev/null 2>&1 || { echo "Missing dependency: $1" >&2; exit 1; }; }

json_get_deepgram_transcript() {
  if command -v jq >/dev/null 2>&1; then
    jq -r '.results.channels[0].alternatives[0].transcript // empty'
  else
    python3 -c 'import json,sys; print(json.load(sys.stdin).get("results",{}).get("channels",[{}])[0].get("alternatives",[{}])[0].get("transcript", ""))'
  fi
}

json_get_text_field() {
  if command -v jq >/dev/null 2>&1; then
    jq -r '.text // empty'
  else
    python3 -c 'import json,sys; print(json.load(sys.stdin).get("text", ""))'
  fi
}

json_get_openai_content() {
  if command -v jq >/dev/null 2>&1; then
    jq -r '.choices[0].message.content // empty'
  else
    python3 -c 'import json,sys; print(json.load(sys.stdin).get("choices",[{}])[0].get("message",{}).get("content", ""))'
  fi
}

make_chat_json() {
  local model=$1
  local prompt=$2
  python3 - "$model" "$prompt" <<'PY'
import json, sys
model, prompt = sys.argv[1], sys.argv[2]
print(json.dumps({
    "model": model,
    "messages": [
        {"role": "system", "content": "You are a helpful language teaching assistant. Be selective, concise, and practical. Always preserve the learner's transcription in your reasoning and focus on high-value vocabulary only."},
        {"role": "user", "content": prompt},
    ],
    "temperature": 0.2,
}))
PY
}

language_name_from_code() {
  case "$LANGUAGE" in
    de|de-*) echo German ;;
    en|en-*) echo English ;;
    es|es-*) echo Spanish ;;
    fr|fr-*) echo French ;;
    it|it-*) echo Italian ;;
    pt|pt-*) echo Portuguese ;;
    *) echo "$LANGUAGE" ;;
  esac
}

known_words_text() {
  if [[ -n "${KNOWN_WORDS_FILE:-}" && -r "$KNOWN_WORDS_FILE" ]]; then
    grep -vE '^[[:space:]]*(#|$)' "$KNOWN_WORDS_FILE" | awk 'BEGIN { sep="" } { printf "%s%s", sep, $0; sep=", " } END { print "" }'
  fi
}

make_vocab_prompt() {
  local transcript=$1
  local known_words
  known_words=$(known_words_text || true)
  cat <<EOF
You are a ${TARGET_LANGUAGE} language teacher preparing a student around ${LEARNER_LEVEL} level.

Here is the transcription from audio the student is listening to. The application has already shown this transcription to the student; use it as the source text and do not ignore it.

Transcription:
---
$transcript
---

Extract vocabulary this ${LEARNER_LEVEL} learner would genuinely benefit from. Be SELECTIVE — at most 6–8 items per chunk. Quality over quantity. If the chunk is too easy or too fragmentary, say "No high-value vocabulary in this chunk." rather than padding.

For each useful item, provide:
- word: canonical form. For German nouns, include der/die/das and plural suffix, e.g. "die Bedeutung, -en". For irregular/separable verbs, include useful hints, e.g. "anfangen, fängt an, fing an, hat angefangen".
- type: gender for nouns or part of speech; mark separable verbs as "verb, trennbar".
- meaning: short English meaning.
- example: quote the sentence or phrase from the transcription where it appears, plus a concise English translation.

Skip obvious A1/A2 material and function words: core verbs, modal verbs, pronouns, articles, most prepositions, question words, numbers, colors, days/months, and very basic adjectives/adverbs.

Prefer genuinely ${LEARNER_LEVEL}-useful items:
- prefix/separable verbs where the meaning is not obvious
- conjunctions and modal particles with real nuance
- abstract nouns for emotions, relationships, social processes
- non-obvious compounds
- idioms/fixed collocations
- false friends or familiar-looking words used non-obviously
- topic-specific vocabulary needed for this scene
EOF

  if [[ -n "$known_words" ]]; then
    cat <<EOF

Do NOT include words the student has already mastered:
$known_words
EOF
  fi

  if [[ "${DISPLAY_MODE:-normal}" == watch ]]; then
    cat <<'EOF'

Output format for a tiny TV overlay:
- Do NOT repeat the full transcription.
- Return at most 4 compact bullets.
- Each bullet should fit on one line: "• word — meaning; note".
- Prefer words that help immediate comprehension of this scene.
- If nothing is worth showing, output exactly: "No high-value vocabulary."
EOF
  else
    cat <<'EOF'

Output format:
- Start with one line: "Transcription shown above." Do not repeat the full transcription.
- Then a short bullet list of vocabulary items.
- Keep the whole answer compact.
EOF
  fi
}

list_sources() {
  need pactl
  echo 'Default sink:'
  pactl get-default-sink || true
  echo
  echo 'Sources:'
  pactl list short sources
  echo
  echo 'Monitor sources:'
  pactl list short sources | awk '$2 ~ /\.monitor$/ {print $2}'
}

choose_audio_source() {
  if [[ -n "${AUDIO_SOURCE:-}" ]]; then
    printf '%s\n' "$AUDIO_SOURCE"
    return
  fi
  need pactl
  local sink
  sink=$(pactl get-default-sink)
  printf '%s.monitor\n' "$sink"
}

record_chunk() {
  local wav=$1
  local source=$2
  need ffmpeg
  log "Recording ${CHUNK_SECONDS}s from ${source}"
  ffmpeg -hide_banner -loglevel error -nostdin -y \
    -f pulse -i "$source" \
    -t "$CHUNK_SECONDS" \
    -ac 1 -ar 16000 \
    "$wav"
}

transcribe_deepgram() {
  local wav=$1
  need curl
  need python3
  : "${DEEPGRAM_API_KEY:?Set DEEPGRAM_API_KEY for STT=deepgram}"

  local url="https://api.deepgram.com/v1/listen?model=${DG_MODEL}&smart_format=true&punctuate=true&language=${LANGUAGE}"
  curl -fsS --request POST \
    --url "$url" \
    --header "Authorization: Token ${DEEPGRAM_API_KEY}" \
    --header 'Content-Type: audio/wav' \
    --data-binary "@${wav}" \
    | json_get_deepgram_transcript
}

transcribe_groq() {
  local wav=$1
  need curl
  need python3
  : "${GROQ_API_KEY:?Set GROQ_API_KEY for STT=groq}"

  local url="${GROQ_BASE_URL}/audio/transcriptions"
  local curl_args=(
    -fsS --request POST
    --url "$url"
    --header "Authorization: Bearer ${GROQ_API_KEY}"
    --form "file=@${wav}"
    --form "model=${GROQ_STT_MODEL}"
    --form "response_format=json"
  )
  if [[ -n "${LANGUAGE:-}" ]]; then
    curl_args+=(--form "language=${LANGUAGE}")
  fi
  if [[ -n "${GROQ_STT_PROMPT:-}" ]]; then
    curl_args+=(--form "prompt=${GROQ_STT_PROMPT}")
  fi

  curl "${curl_args[@]}" | json_get_text_field
}

transcribe_whisper_cpp() {
  local wav=$1
  local out_base=$2
  local bin
  bin=$(command -v whisper-cli || true)
  if [[ -z "$bin" ]]; then
    bin=$(command -v main || true)
  fi
  [[ -n "$bin" ]] || return 1
  [[ -f "$WHISPER_MODEL" ]] || {
    echo "whisper.cpp model not found: $WHISPER_MODEL" >&2
    return 1
  }

  "$bin" -m "$WHISPER_MODEL" -f "$wav" -nt -otxt -of "$out_base" >/dev/null 2>&1
  [[ -s "${out_base}.txt" ]] || return 1
  cat "${out_base}.txt"
}

transcribe_python_whisper() {
  local wav=$1
  local out_dir=$2
  command -v whisper >/dev/null 2>&1 || return 1

  whisper "$wav" \
    --model "$WHISPER_MODEL_NAME" \
    --language "$LANGUAGE" \
    --output_format txt \
    --output_dir "$out_dir" \
    --fp16 False >/dev/null 2>&1

  local txt
  txt=$(find "$out_dir" -maxdepth 1 -type f -name '*.txt' -print -quit)
  [[ -n "$txt" && -s "$txt" ]] || return 1
  cat "$txt"
}

transcribe_whisper() {
  local wav=$1
  local tmp=$2
  if transcribe_whisper_cpp "$wav" "$tmp/whisper-out"; then
    return 0
  fi
  if transcribe_python_whisper "$wav" "$tmp"; then
    return 0
  fi

  cat >&2 <<EOF
No local Whisper command worked.
Options:
  1. whisper.cpp: install `whisper-cli` and set WHISPER_MODEL=/path/to/ggml-model.bin
  2. Python Whisper: pip install -U openai-whisper, then use WHISPER_MODEL_NAME=base.en
  3. Custom: STT=cmd TRANSCRIBE_CMD='your-command {audio}'
EOF
  return 1
}

transcribe_cmd() {
  local wav=$1
  : "${TRANSCRIBE_CMD:?Set TRANSCRIBE_CMD for STT=cmd, using {audio} as placeholder}"
  local q_wav
  printf -v q_wav '%q' "$wav"
  local cmd=${TRANSCRIBE_CMD//\{audio\}/$q_wav}
  bash -lc "$cmd"
}

transcribe() {
  local wav=$1
  local tmp=$2
  case "$STT" in
    deepgram) transcribe_deepgram "$wav" ;;
    groq)     transcribe_groq "$wav" ;;
    whisper)  transcribe_whisper "$wav" "$tmp" ;;
    cmd)      transcribe_cmd "$wav" ;;
    *) echo "Unknown STT backend: $STT" >&2; exit 1 ;;
  esac
}

explain_ollama() {
  local prompt=$1
  need ollama
  printf '%s\n' "$prompt" | ollama run "$OLLAMA_MODEL"
}

explain_openai() {
  local prompt=$1
  need curl
  need python3
  : "${OPENAI_API_KEY:?Set OPENAI_API_KEY for LLM=openai}"

  make_chat_json "$OPENAI_MODEL" "$prompt" | curl -fsS "${OPENAI_BASE_URL}/chat/completions" \
    --header "Authorization: Bearer ${OPENAI_API_KEY}" \
    --header 'Content-Type: application/json' \
    --data-binary @- \
    | json_get_openai_content
}

explain_groq() {
  local prompt=$1
  need curl
  need python3
  : "${GROQ_API_KEY:?Set GROQ_API_KEY for LLM=groq}"

  make_chat_json "$GROQ_MODEL" "$prompt" | curl -fsS "${GROQ_BASE_URL}/chat/completions" \
    --header "Authorization: Bearer ${GROQ_API_KEY}" \
    --header 'Content-Type: application/json' \
    --data-binary @- \
    | json_get_openai_content
}

explain() {
  local transcript=$1
  local prompt
  prompt=$(make_vocab_prompt "$transcript")
  case "$LLM" in
    none) return 0 ;;
    ollama) explain_ollama "$prompt" ;;
    groq) explain_groq "$prompt" ;;
    openai) explain_openai "$prompt" ;;
    *) echo "Unknown LLM backend: $LLM" >&2; exit 1 ;;
  esac
}

write_latest_file() {
  local transcript=$1
  local explanation=${2:-}
  [[ -n "${LATEST_FILE:-}" ]] || return 0

  local latest_dir latest_tmp
  latest_dir=$(dirname "$LATEST_FILE")
  mkdir -p "$latest_dir"
  latest_tmp=$(mktemp "${LATEST_FILE}.tmp.XXXXXX")

  {
    printf 'Updated: %(%H:%M:%S)T\n\n' -1
    printf 'Transcription\n%s\n' "$transcript"
    if [[ -n "$explanation" ]]; then
      printf '\nVocabulary\n%s\n' "$explanation"
    fi
  } >"$latest_tmp"

  mv "$latest_tmp" "$LATEST_FILE"
}

print_update() {
  local transcript=$1
  local explanation=${2:-}

  if [[ "$DISPLAY_MODE" == watch ]]; then
    printf '\033[2J\033[H'
    printf 'Updated: %(%H:%M:%S)T\n\n' -1
    printf 'Transcription\n%s\n' "$transcript"
    if [[ -n "$explanation" ]]; then
      printf '\nVocabulary\n%s\n' "$explanation"
    fi
    printf '\n'
    return 0
  fi

  printf '\n========== Transcription ==========\n%s\n' "$transcript"
  if [[ -n "$explanation" ]]; then
    printf '\n========== Vocab help (%s) ==========\n%s\n' "$LLM" "$explanation"
  fi
}

process_audio_file() {
  local wav=$1
  local tmp
  tmp=$(mktemp -d)

  log "Transcribing with STT=${STT}: $(basename "$wav")"
  local transcript
  if ! transcript=$(transcribe "$wav" "$tmp" | tr '\n' ' ' | sed -E 's/[[:space:]]+/ /g; s/^ //; s/ $//'); then
    log "Transcription failed for $(basename "$wav")"
    rm -rf "$tmp"
    return 0
  fi

  if [[ -z "$transcript" ]]; then
    log 'No transcript returned.'
    rm -rf "$tmp"
    return 0
  fi

  # In watch mode, update the overlay as soon as the transcription is available.
  # The LLM explanation usually arrives later, so this keeps the display useful
  # while the episode is moving on.
  if [[ "$DISPLAY_MODE" == watch ]]; then
    write_latest_file "$transcript" ""
    print_update "$transcript" ""
  fi

  local explanation=""
  if [[ "$LLM" != none ]]; then
    if ! explanation=$(explain "$transcript"); then
      log "LLM explanation failed for $(basename "$wav")"
      explanation=""
    fi
  fi

  write_latest_file "$transcript" "$explanation"
  print_update "$transcript" "$explanation"

  rm -rf "$tmp"
}

run_one_chunk() {
  local source=$1
  local tmp
  tmp=$(mktemp -d)

  local wav="$tmp/chunk.wav"
  record_chunk "$wav" "$source"
  process_audio_file "$wav"

  rm -rf "$tmp"
}

run_segmented() {
  local source=$1
  local tmp
  tmp=$(mktemp -d)

  local pattern="$tmp/chunk_%06d.wav"
  log "Recording continuously from ${source}; processing ${CHUNK_SECONDS}s chunks as they complete"
  ffmpeg -hide_banner -loglevel error -nostdin -y \
    -f pulse -i "$source" \
    -ac 1 -ar 16000 \
    -f segment \
    -segment_time "$CHUNK_SECONDS" \
    -reset_timestamps 1 \
    "$pattern" &

  local ffmpeg_pid=$!

  cleanup_segmented() {
    kill "$ffmpeg_pid" 2>/dev/null || true
    wait "$ffmpeg_pid" 2>/dev/null || true
    rm -rf "$tmp"
  }
  trap cleanup_segmented EXIT
  trap 'trap - EXIT INT TERM; cleanup_segmented; exit 130' INT
  trap 'trap - EXIT INT TERM; cleanup_segmented; exit 143' TERM

  local files count i
  while kill -0 "$ffmpeg_pid" 2>/dev/null; do
    mapfile -t files < <(find "$tmp" -maxdepth 1 -type f -name 'chunk_*.wav' | sort)
    count=${#files[@]}

    if [[ "$DISPLAY_MODE" == watch && $count -gt $((WATCH_MAX_COMPLETED_CHUNKS + 1)) ]]; then
      local drop_count=$((count - WATCH_MAX_COMPLETED_CHUNKS - 1))
      for ((i = 0; i < drop_count; i++)); do
        log "Dropping stale chunk: $(basename "${files[$i]}")"
        rm -f "${files[$i]}"
      done
      files=("${files[@]:$drop_count}")
      count=${#files[@]}
    fi

    # ffmpeg is still writing the newest segment.  Process only older files so
    # transcription never races a partially-written wav.
    if (( count > 1 )); then
      for ((i = 0; i < count - 1; i++)); do
        process_audio_file "${files[$i]}"
        rm -f "${files[$i]}"
      done
    fi

    sleep "$POLL_SECONDS"
  done

  wait "$ffmpeg_pid" 2>/dev/null || true
  trap - EXIT INT TERM
  cleanup_segmented
}

main() {
  local cmd=${1:-start}

  CHUNK_SECONDS=${CHUNK_SECONDS:-5}
  LANGUAGE=${LANGUAGE:-en}
  TARGET_LANGUAGE=${TARGET_LANGUAGE:-$(language_name_from_code)}
  LEARNER_LEVEL=${LEARNER_LEVEL:-B1}
  DISPLAY_MODE=${DISPLAY_MODE:-normal}
  POLL_SECONDS=${POLL_SECONDS:-1}
  WATCH_MAX_COMPLETED_CHUNKS=${WATCH_MAX_COMPLETED_CHUNKS:-1}
  STT=${STT:-deepgram}
  LLM=${LLM:-none}
  DG_MODEL=${DG_MODEL:-nova-3}
  GROQ_BASE_URL=${GROQ_BASE_URL:-https://api.groq.com/openai/v1}
  GROQ_STT_MODEL=${GROQ_STT_MODEL:-whisper-large-v3-turbo}
  WHISPER_MODEL=${WHISPER_MODEL:-$HOME/models/ggml-base.en.bin}
  WHISPER_MODEL_NAME=${WHISPER_MODEL_NAME:-base.en}
  OLLAMA_MODEL=${OLLAMA_MODEL:-llama3.1}
  GROQ_MODEL=${GROQ_MODEL:-llama-3.3-70b-versatile}
  OPENAI_BASE_URL=${OPENAI_BASE_URL:-https://api.openai.com/v1}
  OPENAI_MODEL=${OPENAI_MODEL:-gpt-4o-mini}

  case "$cmd" in
    help|-h|--help) usage ;;
    sources) list_sources ;;
    once)
      local source
      source=$(choose_audio_source)
      run_one_chunk "$source"
      ;;
    start)
      local source
      source=$(choose_audio_source)
      log "Starting. Press Ctrl-C to stop. STT=${STT}, LLM=${LLM}, chunk=${CHUNK_SECONDS}s"
      run_segmented "$source"
      ;;
    *) echo "Unknown command: $cmd" >&2; usage >&2; exit 1 ;;
  esac
}

main "$@"
