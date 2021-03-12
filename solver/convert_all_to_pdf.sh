 #!/bin/bash
for i in *.html ; do 
    wkhtmltopdf --enable-local-file-access -L 15mm -R 15mm -T 15mm -B 15mm "$i" "$(basename "${i/.html}")".pdf
done
