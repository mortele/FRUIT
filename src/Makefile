#------------------------
#
# Author: Andrew H. Chen chen@meihome.com
# Last modified: 2004/01/12
# Version : $Revision$
#------------------------

compile:
	ifort -c FruitUtil.f90
	ifort -c Fruit.f90
	ifort -c FruitTest.f90
	ifort -c FruitUtilTest.f90
	ifort -c AllUnitTest.f90
	ifort -c Main.f90
	ifort -o fruittest *.o

clean:
	rm -rf *.o *.mod fruittest
	rm -rf fruittest

test:
	./fruittest

all: clean compile test 

