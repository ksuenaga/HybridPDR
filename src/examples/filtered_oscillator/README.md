This benchmark is taken from [http://spaceex.imag.fr/download-6].
The following is the original README.txt file.

==
This benchmark models a two-dimensional switched oscillator connected to 
filter that smoothes the output signal. The models are parameterized 
over the order (number of 1st-order differential equations) of the
filter.

The oscillator is modeled with 4 locations and 2 continuous variables,
x and y. The filter is a purely continuous system that takes as input 
the value of x, and outputs a smoothened signal z, which depends on x 
via a k-th order system of 1st-order linear differental equations.
The combined system has 4 locations and 2+k continous variables.

Running the example
-------------------

The example can be run by opening a model (xml) file and its corresponding
config (cfg) file in the web interface. The model file filtered_oscillator.xml 
includes the models up to order 96.

Alternatively, from the command line use, e.g.,
	spaceex --config filtered_oscillator.128th_order.cfg -m filtered_oscillator_128.xml -o out.txt

To show the flowpipe-errors for each iteration, use verbosity level D4 or higher.
	
Results
-------
On a standard PC we obtain the following performance results for full 
reachability up to a fixpoint.

Fixed time-step 0.05

Directions	variables	time(s)		memory(MB)	Iterations		time/iter(s)	flowpipe-error
[box]											up to fixpoint					max
----------------------------------------------------------------------------------------------
20			10			0,7			7,5			7				0,10			0,010
36			18			2,0			9,3			9				0,22            0,010
68			34			9,1			20,2		13				0,70            0,010
132			66			77,3		50,3		23				3,36            0,013
196			98			367,3		105,6		31				11,85           0,019
260			130			1185,6		194,3		39				30,40           0,030
396			198			7822,5		511,0		57				137,24          0,074
