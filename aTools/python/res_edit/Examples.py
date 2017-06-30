##Python examples:
    
##fileinput already supports inplace editing. It redirects stdout to the file in this case:

import fileinput

for line in fileinput.input(fileToSearch, inplace=True):
    print(line.replace(textToSearch, textToReplace), end='')

    
