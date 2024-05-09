update:
	cp ~/.emacs.d/configs/move.el .

test:
	emacs test

.PHONY: test update
