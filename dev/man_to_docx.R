# First make the manual in pdf:
usethis::build_manual()

# Converting from pdf to docx using python:
library(reticulate)
# install_python()
#
py_install("pdf2docx")
py_run_string("from pdf2docx import parse")

# path of pdf file
py_run_string("pdf_file = 'C:/Users/g_del/Documents/R/SAiVE_1.0.6.pdf'")

# will create .docx in same path
py_run_string("docx_file = 'C:/Users/g_del/Documents/R/SAiVE_1.0.6.docx'")

# Here is where we convert pdf to docx
py_run_string("parse(pdf_file, docx_file, start=0, end=None)")
