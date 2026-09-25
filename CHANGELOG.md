# Revision history for Agility

## Unreleased

* Image tables use Agility's own PNG decoder and renderer instead of JuicyPixels, chafa and img2txt: full PNG support including transparency, 24-bit colour and smoother scaling.
* Images are fetched and decoded on the source thread instead of while drawing.
* Added a test suite, run with `stack test`.

## 0.1.0.0 -- 2026-03-09

* First version. Mostly a skeleton.
