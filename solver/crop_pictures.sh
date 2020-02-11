 #!/bin/bash
for f in *.jpg
do
	convert $f -trim $f
done
