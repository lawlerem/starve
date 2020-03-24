all: staRVe.so family.so
	#
staRVe.so: staRVe.cpp
	Rscript --vanilla -e "TMB::compile('staRVe.cpp')"

family.so: family.cpp
	Rscript --vanilla -e "TMB::compile('family.cpp')"

clean:
	rm -rf *.o
