def list1= ["README.md", "test1.txt","/desktop/openmpi.txt","main.go"]
def bool=list1.every { it =~ /(?i)\.(?:md|txt)$/ }
print(bool)  