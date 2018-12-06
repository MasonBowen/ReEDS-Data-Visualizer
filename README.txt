This code is based heavily/entirely on the work of Yinong Sun and is being adapted for ReEDS-2.0 India by Thomas Bowen.

The original can be found in the main ReEDS Github Repo.

Started: 11/15/2018


----------------------------------------------------
----------------------------------------------------
######	Notes and potential sources of error  ######
----------------------------------------------------
----------------------------------------------------

1) Gams Directory Issues
When trying to find the version of gams in use, the original code pointed to 'C:/' directly, new code points to 'C:/Program Files (86x)' (as that's where mine is);
In order to simplify the adaptation of this for other users, I am using an R Project ("InteractiveDataVisualizer") which helps keep the directory paths in order;
However, to jump up to the 'C:/Program Files (86x)' from the RProject directory I use an arbitrary number of "../"'s which should take the user to the C drive but may be insufficient.

2) INR cost coversions
In the "\inout\R\readwrite.R" function costs are converted (for the objective function and elsewhere) from thousands to billions, need to find out what the standard output for ReEDS-India objective function is

3) AC-DC
In the "\inout\R\readwrite.R" function solar is converted from DC to AC, do we need to do that post processing? Or does it happen in the Supply file or elsewhere?

4) Plotting functions 'templates.R'
"\inout\R\templates.R" is going to need a LOT of reworking and trimming back ... just a heads up

5) Present Value Factors
In "\inout\R\auxillary.R" there is a function 'present_value_factors' which has a lot of specific values which will need to be rechecked and could possibly be exported to csv's for easier manipulation

6) Input csv's 
Need to go and double check all of the tech types available, etc. and adjust the csv's appropriately

7) Relative r Project - gdx file location
This assumes that the r project exists in a folder which is at the same 'level' as the 'capexp-india' folder

8) '.gdx.structure'
a)
This is a key list (defined in "inout/R/reeds_function.R") which determines the overall structure of all the gdx files to be read in. This needs to be adapted to our ReEDS - India model.
Unfortunately, it is defined in Yinong's version using .gdx files which I can find in the Y drive ("Y:\6A20\Public\SCohen\CIRA\ReEDS Results\CIRA_Ref_to2050_v2016FR_20160725\gdxfiles"),
but of whose origin and purpose I am unsure. Need to dif into actual .gms files to see how they are created. Then need to create equivalent .gdx files for India ReEDS model and 
change .gdx.structure appropriately.

b)
Only including variables in '.gdx.structure' for now

9) creating 'gdx_str_file.csv'
happens in 'gdx_str_generator.R', build out based on random selection of a gdx file, assumes that all gdx files are of same basic structure, may not be the case

10) 