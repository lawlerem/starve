all: staRVe.so predict_w.so family.so
	#
staRVe.so: staRVe.cpp
	Rscript --vanilla -e "TMB::compile('staRVe.cpp')"

predict_w.so: predict_w.cpp
	Rscript --vanilla -e "TMB::compile('predict_w.cpp')"

family.so: family.cpp
	Rscript --vanilla -e "TMB::compile('family.cpp')"
