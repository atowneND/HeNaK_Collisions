x=2
filename="dat2832forpy.dat"

origdat=open(filename)
print "name =",origdat.name
lineno=0
for line in origdat:
    lineno += 1
origdat.close()
print x
