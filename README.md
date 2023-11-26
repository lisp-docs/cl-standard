# README

## Sources

The sources for this project are the Public Domain sources dpANS3r. 

Please see the files in the repository. 

Included as well are previous Public Domain versions of the text including dpANS1, dpANS2, and dpANS3. 

Note that dpANS3r is a set of modifications to dpANS3, so to get the actual text from the original files, you must first untar dpANS3 and then replace the corresponding files changed in the untared dpANS3r.

## How to Replicate

Originally the goal of this project was to parse the TeX source files and produce MarkDown while maintaining as much as possible of the structure of the text. This proved to be overwhelmingly time consuming. Instead, this was the process followed:

1. Compile the original TeX files to PDF or just get a PDF from the Public Domain ones available online.
2. Using `pandoc` convert the files to `.docx`. This somehow was useful in preserving some of the structure of the files. Google Docs can also be used to convert the `pdf` files into `docx` files.
3. Convert the `docx` files into MakrDown files `.md` using `pandoc`. Please note that there are different `pandoc` options that will produce different results in this step.
4. Process the obtained markdown with the lisp file that I will soon upload.

For clarity and reproducibility, even though I did not specify above the exact `pandoc` options used, I included the files from each step in the repository.
