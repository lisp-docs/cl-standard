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

1. The original `pdf` files used are located in: [/new-spec/spec-source-pdf/chapters]
2. I used Google Docs (because had some issue with pandocs at the time...) to convert the `pdf` files to `docx` format. They are located here: [/new-spec/spec-source-pdf/output/chapters/docx]
3. The markdown's were produced with an online free conversion app that uses pandocs. They are here: [/new-spec/spec-source-pdf/output/chapters/md]
4. The code to process the resulting markdown files from step 3 into broken up files, escaped for MDX (React MarkDown), and other details like producing MarkDown headings instead of just bolded sentences, etc, is here: [https://github.com/lisp-docs/process-dpans3r]
