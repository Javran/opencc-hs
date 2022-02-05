# Revision history for opencc-hs

## 0.1.1.0 -- 2022-2-5

* Added `opencc` to `extra-libraries` in the cabal file to avoid surprises that a downstream consumer of this binding has to link opencc manually.
* Relaxed version upper bounds.

## 0.1.0.0 -- 2021-05-18

* Conversion functions now take 'Data.Text's and return 'Data.Text's, assuming the input text is valid UTF-8.

## 0.1.0.0 -- 2021-05-17

* First release of OpenCC-hs. It features raw bindings and three styles of higher-level bindings: (1) one-shot, (2) IO handle, (3) monadic.
