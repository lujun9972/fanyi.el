;;; fanyi.el --- Core translation library using large language models
;; Author: DarkSun <lujun9972@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (s "1.12.0") (llm "0.22.0") )
;; URL: https://github.com/lujun9972/fanyi.el
;; Keywords: translation, tools

;;; Commentary:
;; Fanyi is an AI-powered translation toolkit for Emacs.
;;
;; Features:
;; - Support for multiple LLM providers
;; - Two-step translation refinement

;;; Code:

(require 's)
(require 'llm)

(defgroup fanyi nil
  "AI-powered translation toolkit."
  :group 'tools
  :prefix "fanyi-")

(defcustom fanyi-baseurl "https://open.bigmodel.cn/api/paas/v4"
  "Base URL for LLM API.")

(defcustom fanyi-model "glm-4-flash"
  "Model name for translation.")
(defcustom fanyi-key (getenv "FANYI_LLM_KEY")
  "API key for authentication."
  :type 'string
  :group 'fanyi)

(defcustom fanyi-temperature 0.4
  "Temperature parameter for LLM generation."
  :type 'float
  :group 'fanyi)

(defcustom fanyi-llm-provider nil
  "LLM provider configuration.
See `make-llm-openai-compatible' for configuration details.")


(defun fanyi-do-completion (prompt &optional role)
  "Execute LLM completion with PROMPT and ROLE context."
  (let ((llm-warn-on-nonfree nil)
        (provider (or fanyi-llm-provider
                      (make-llm-openai-compatible :key fanyi-key :url fanyi-baseurl :chat-model fanyi-model)))
        (prompt (llm-make-chat-prompt prompt :context role :temperature fanyi-temperature)))
    (llm-chat provider prompt)))

;; prompt来源: https://mp.weixin.qq.com/s/xsa7hUv0746jDmy9YdLx1w

(defun fanyi-initial-translation (source-text &optional source-lang target-lang )
  "Generate initial translation of SOURCE-TEXT.
该函数是一个简单的翻译工具，它利用大型语言模型（LLM）将一段文本(source_text)从一种语言(source_lang)翻译成另一种语言(target_lang).
函数接收source_lang、 target_lang和source_text作为输入，构造一个system_message 来设定 LLM 的角色，然后创建一个明确的translation_prompt ，要求 LLM 仅提供翻译结果而不添加任何额外内容。最后，函数通过调用 get_completion 函数来获取 LLM 的翻译输出并返回。"
  (let* ((source-lang (or source-lang "English"))
         (target-lang (or target-lang "Chinese"))
         (role (s-lex-format "You are an expert linguist, specializing in translation from ${source-lang} to ${target-lang}."))
         (prompt (s-lex-format "This is an ${source-lang} to ${target-lang} translation, please provide the ${target-lang} translation for this text.
Do not provide any explanations or text apart from the translation.
${source-lang}: ${source-text}

${target-lang}:"))
         (translation (fanyi-do-completion prompt role)))
    (message "初次翻译:[%s]" translation)
    translation))

(defun fanyi-reflect-on-translation (source-text translation-1 &optional source-lang target-lang country)
  "Generate improvement suggestions for TRANSLATION-1 of SOURCE-TEXT.
该函数利用大型语言模型（LLM）对一段文本的翻译进行反思和评估，以提高翻译质量。函数接收源语言（source_lang）、目标语言（target_lang）、原文本（source_text）、初始翻译（translation_1）以及可选的国家/地区参数（country），构造一个system_message 来设定 LLM 的角色为翻译专家，然后根据是否提供国家/地区信息来生成相应的反思和建议。reflection_prompt 中明确要求 LLM 从准确性、流畅性、风格和术语使用等方面对翻译进行评估，并提出具体的改进建议。最后，函数通过调用 get_completion 函数获取 LLM 的反思结果并返回。该函数适用于需要对翻译结果进行优化的场景，尤其是当需要针对特定地区语言变体进行翻译优化时。"
  (let* ((source-lang (or source-lang "English"))
         (target-lang (or target-lang "Chinese"))
         (role "You are an expert linguist specializing in translation from ${source-lang} to ${target-lang}.
You will be provided with a source text and its translation and your goal is to improve the translation.")
         (country-prompt (if country
                             (s-lex-format "The final style and tone of the translation should match the style of ${target-lang} colloquially spoken in {country}.")
                           ""))
         (prompt (s-lex-format "Your task is to carefully read a source text and a translation from ${source-lang} to ${target-lang}, and then give constructive criticisms and helpful suggestions to improve the translation.

${country-prompt}

The source text and initial translation, delimited by XML tags <SOURCE_TEXT></SOURCE_TEXT> and <TRANSLATION></TRANSLATION>, are as follows:

<SOURCE_TEXT>
${source-text}
</SOURCE_TEXT>

<TRANSLATION>
${translation-1}
</TRANSLATION>

When writing suggestions, pay attention to whether there are ways to improve the translation's
(i) accuracy (by correcting errors of addition, mistranslation, omission, or untranslated text),
(ii) fluency (by applying {target_lang} grammar, spelling and punctuation rules, and ensuring there are no unnecessary repetitions),
(iii) style (by ensuring the translations reflect the style of the source text and take into account any cultural context),
(iv) terminology (by ensuring terminology use is consistent and reflects the source text domain; and by only ensuring you use equivalent idioms ${target-lang}).

Write a list of specific, helpful and constructive suggestions for improving the translation.
Each suggestion should address one specific part of the translation.
Output only the suggestions and nothing else."))
         (reflection (fanyi-do-completion prompt role)))
    (message "对初翻给出的建议:[%s]" reflection)
    reflection))

(defun fanyi-improve-translation (source-text translation-1 reflection &optional source-lang target-lang)
  "Generate improved translation based on REFLECTION feedback.
该函数利用大型语言模型（LLM）根据reflection种给出的建议对source_text文本的翻译（translation_1）进行改进。函数接收source_lang、target_lang、source_text、translation_1、reflection作为输入，构造一个system_message 来设定 LLM 的角色为翻译专家，然后创建一个详细的prompt ，要求 LLM 根据专家建议对翻译进行编辑，确保翻译的准确性、流畅性、风格和术语使用等方面得到优化。最后，函数通过调用 get_completion 函数获取改进后的翻译结果并返回。该函数适用于需要对翻译结果进行精细化调整的场景，尤其是当有具体的改进建议时，能够有效提升翻译质量。"
  (let* ((source-lang (or source-lang "English"))
         (target-lang (or target-lang "Chinese"))
         (role (s-lex-format "You are an expert linguist, specializing in translation editing from ${source-lang} to ${target-lang}."))
         (prompt (s-lex-format "Your task is to carefully read, then edit, a translation from ${source-lang} to ${target-lang}, taking into account a list of expert suggestions and constructive criticisms.

The source text, the initial translation, and the expert linguist suggestions are delimited by XML tags <SOURCE_TEXT></SOURCE_TEXT>, <TRANSLATION></TRANSLATION> and <EXPERT_SUGGESTIONS></EXPERT_SUGGESTIONS> as follows:

<SOURCE_TEXT>
${source-text}
</SOURCE_TEXT>

<TRANSLATION>
${translation-1}
</TRANSLATION>

<EXPERT_SUGGESTIONS>
${reflection}
</EXPERT_SUGGESTIONS>

Please take into account the expert suggestions when editing the translation. Edit the translation by ensuring:

(i) accuracy (by correcting errors of addition, mistranslation, omission, or untranslated text),
(ii) fluency (by applying {target_lang} grammar, spelling and punctuation rules and ensuring there are no unnecessary repetitions),
(iii) style (by ensuring the translations reflect the style of the source text)
(iv) terminology (inappropriate for context, inconsistent use), or
(v) other errors.

Output only the new translation and nothing else."))
         (translation-2 (fanyi-do-completion prompt role)))
    (message "最终翻译结果[%s]" translation-2)
    translation-2))

;;;###autoload
(defun fanyi-content (content &optional source-lang target-lang country)
  "Translate CONTENT from SOURCE-LANG to TARGET-LANG with optional COUNTRY variant.
When called interactively, uses the active region or current paragraph."
  (interactive
   (list (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (thing-at-point 'paragraph t))
         nil nil nil))
  (let* ((source-lang (or source-lang "English"))
         (target-lang (or target-lang "Chinese"))
         (translation-1 (fanyi-initial-translation content source-lang target-lang))
         (reflection (fanyi-reflect-on-translation content translation-1 source-lang target-lang country)))
    (fanyi-improve-translation content translation-1 reflection source-lang target-lang)))

(provide 'fanyi)
;;; fanyi.el ends here
