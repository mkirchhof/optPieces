This folder includes all functions that summarize or further refine 
the result of the sheet analysis  and save them into a file. 
You can add your own functions as files into this folder
which are then automatically detected and available in the tool.

Each file should include only one function and not run code on its own. Comment out testcode etc.

The function inside the file should have the following structure:
Input: object created by the tool that includes all data ($data), user limits ($params) 
and further information ($extraInfo)
Output: nothing, but save a file somewhere in the process.

BE AWARE that your code is not futher checked before running it! That means that the tool
does allow to override its own functions if you include malicious code. If you find the tool
crashing after adding your file, remove the file from this folder and check it for mistakes.
Malfunctions may arise from refering to objects outside the function's environment.