EMACS = emacs
ERT = ~/lib/emacs/ert

EMACS_ERT_PFX = cask exec $(EMACS) --batch --quick --directory=$(ERT) --load=ert.el
EMACS_ERT_SFX = --funcall=ert-run-tests-batch-and-exit

ERT_TESTS := $(filter-out ert-%,$(wildcard *.el))

test:
	$(EMACS_ERT_PFX) -L . $(addprefix -l ,$(ERT_TESTS)) $(EMACS_ERT_SFX)

docker-test:
	docker run -v $(shell pwd):/opt silex/emacs:27.2-ci-cask sh -c "cd /opt; make"
