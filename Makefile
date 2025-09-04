PYTHON ?= python3
export PYTHONPATH := src

.PHONY: test run

run:
	$(PYTHON) .\src\setup.py develop
	$(PYTHON) .\src\main.py

test:
	$(PYTHON) .\src\setup.py develop
	$(PYTHON) -m pytest tests