###########################
Instructions for ReEDS Visualization tool - Last update on December 06, 2018 @ 3:00 PM

Please address all comments, complaints, questions and expletives to Thomas Bowen

############################
Packages

The first issue you might run into is in loading the packages. 
The first time this is done it will take some time as you will be installing a few packages with non-insignificant dependencies.
Of these, the most troublesome will be 'qdap' as it depends on rJava.

If you have issues with these you might see an error:
       "Loading required package: qdap
	Error : .onLoad failed in loadNamespace() for 'rJava', details:
	call: fun(libname, pkgname)
	error: JAVA_HOME cannot be determined from the Registry"

This means that R cannot find your copy of Java (either because you have the wrong installation (32-bit vs 64-bit) or 
because you haven't installed it all).

	For more information on this error see the following stackexchange: 
		"https://stackoverflow.com/questions/9120270/how-can-i-install-rjava-for-use-with-64bit-r-on-a-64-bit-windows-computer"

	Essentially you just need to point R to your installation of Java.

	This is currently in the code but commented out (lines 28-39 as of initial edition, below loading 'pacman')


The next issue you might have with is 'units' because it will ask if you want to source from somewhere that needs to compile, 
just select no and it should work

Please let me know if there are any other issues with the packages!




######################
Directories

The next issue you may have is with directories.
You will need to change all of the directories in the 'point to appropriate directories' section
These include:
	
	1) setting the working directory to the file location ABOVE where the r script is, 
	
	2) changing where gams can be found (you will likely need to have a (or several) '/../' to help r escape from it's current working directory

	3) directories for the csv files, functions, server/ui scripts (when you pull the repo they should already be in the proper format)

	4) directory of the gdx files

	5) location and title of the shape file (.shp)




######################
gdx_all_var

One specific note regarding the function 'gdx_all_var' (which uploads all the relevant gdx data):

	If you choose to run this function with the 'parallel' argument equal to TRUE, it will run faster BUT you will need 
	to actually go into the 'gdx_extract_all.R' script and manually change some of the directories 
	(look for the '# CHANGE DIRECTORIES HERE!!!' note on line 18). This happens because when running operations in parallel 
	the clusters cannot see the global environment, so far I cannot figure out a workaround...

#######################
Additional notes

That's all that I can foresee now (although there will certainly be unforeseen issues). 
Regarding questions inside of the shiny tool, the first tab has instructions on it and if that doesn't solve it please contact me.

When we get more funding my next steps (assuming you all do not have more pressing priorities) are to add more informative error 
messages and continue to shift some of the plotting parameters over to csv's so that a non-R-user can easily adjust things as they see fit.

----------

Best of luck, I look forward to your feedback and hope the tool proves useful!!

-Thomas
