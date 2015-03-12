
#sample for gfortran and make.


compiler=gfortran
option=-Wall -Wextra -pedantic -fbounds-check -Wuninitialized -O -g \
 -Wno-unused-parameter -cpp

### fruit_code = ../src/fruit_util.f90 ../src/fruit.f90
fruit_code = ../src/fruit.f90
code = calculator.f90 calculator_test.f90
code_gen = fruit_basket_gen.f90 fruit_driver_gen.f90

all_code = $(fruit_code) $(code) $(code_gen)

driver = fruit_driver.exe

all : build run

build : $(driver)

fruit_basket_gen.f90 : $(code)
	ruby fruit_generator.rb

fruit_driver_gen.f90 : $(code)
	ruby fruit_generator.rb

fruit_driver.exe : $(all_code)
	$(compiler) $(option) $(all_code) -o fruit_driver.exe

clean : 
	rm -f *.o *.mod *.obj *.exe
	rm -f fruit_driver_gen.f90
	rm -f fruit_basket_gen.f90
	rm -f result.xml result_tmp.xml

run : $(driver)
	./$(driver)
	
	

.PHONY : all clean run

