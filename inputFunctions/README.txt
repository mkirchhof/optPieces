This folder includes all functions that transform a raw datafile into a dataframe
interpretable for the tool. You can add your own functions as files into this folder
which are then automatically detected and available in the tool.

Each file should include only one function and not run code on its own. Comment out testcode etc.

The function inside the file should have the following structure:
Input: string that includes the filepath
Output: non-empty dataframe. Rows equal observations, columns give for example
  time, position or measurement value. Please name your columns understandable as
  they will be presented to the user. As a convention, use the following structure:
  "<what is measured><nothing or _time or _position>".

BE AWARE that your code is not futher checked before running it! That means that the tool
does allow to override its own functions if you include malicious code. If you find the tool
crashing after adding your file, remove the file from this folder and check it for mistakes.
Malfunctions may arise from refering to objects outside the function's environment.