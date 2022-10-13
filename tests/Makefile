COMPILER = fpc
OUTPUT = output
PROGRAM = Tests.pas
OPTIONS = -Fu../sources -vm6058 -gw -godwarfcpp -Sa

all:
	mkdir -p ${OUTPUT}
	${COMPILER} ${OPTIONS} -FU${OUTPUT} ${PROGRAM}
	./Tests