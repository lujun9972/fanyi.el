# Fanyi - AI-Powered Translation Toolkit for Emacs

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


An intelligent translation workflow powered by large language models (LLMs), featuring **translation refinement** and **Org mode integration**.

## Features

### Core Translation
- 🚀 Two-step translation process (initial translation + expert refinement)
- 🔧 Support multiple LLM providers (OpenAI/GLM compatible APIs)
- ⚙️ Customizable parameters: 
  - Temperature control
  - Source/target languages
  - Regional variants

### Org Mode Integration
- 📑 Structure-preserving translation of Org documents
- ✅ Supported elements:
  ```text
  Headlines    Comments     Plain Text
  Footnotes    Inlinetasks
  ```

## Installation

### Manual Installation
```bash
git clone https://github.com/lujun9972/fanyi.el.git ~/.emacs.d/fanyi
```
```elisp
;; Init configuration
(add-to-list 'load-path "~/.emacs.d/fanyi")
(require 'fanyi)
```

## Configuration

### API Setup
```elisp
(setq fanyi-key "your-api-key-here")  ; Required
(setq fanyi-baseurl "https://open.bigmodel.cn/api/paas/v4")
(setq fanyi-model "glm-4")           ; Default model
(setq fanyi-temperature 0.4)         ; Creativity level
```

### Org Integration
```elisp
(with-eval-after-load 'org
  (require 'fanyi-org))
```

## Usage

### Basic Translation
| Command                | Description                     | Keybinding |
|------------------------|---------------------------------|------------|
| `fanyi-content`        | Translate text/region           | `C-c t t`  |

### Org Mode Workflow
```elisp
M-x fanyi-org-current-buffer    ;; Translate current buffer
M-x fanyi-org-file      ;; Translate Org file (creates .translated copy)
```

## Example

### Text Translation
```elisp
(fanyi-content "The quick brown fox jumps over the lazy dog.")
;; => "敏捷的棕色狐狸跳过懒惰的狗"
```

### Document Translation
**Input** (`document.org`):
```org
* Meeting Notes
- Review Q2 results [[2024-07-01 Mon]]
- Discuss <<project timeline>>
```

**After Translation** (`document.org.translated`):
```org
* 会议记录
- 回顾第二季度结果 [[2024-07-01 周一]]
- 讨论 <<项目时间表>>
```


## License

MIT License © 2025 DarkSun

```
