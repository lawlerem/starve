all: staRVe.so predict_w.so family.so
	#
staRVe.so: staRVe.cpp
	Rscript --vanilla -e "TMB::compile('staRVe.cpp',flags='-std=c++14')"

predict_w.so: predict_w.cpp
	Rscript --vanilla -e "TMB::compile('predict_w.cpp',flags='-std=c++14')"

family.so: family.cpp
	Rscript --vanilla -e "TMB::compile('family.cpp',flags='-std=c++14')"

clean: rm *.o
