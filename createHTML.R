# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files for exercise 1
knit("PA1_template.Rmd")
markdownToHTML('PA1_template.md', 'PA1_template.html', options=c("use_xhml"))