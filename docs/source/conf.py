# Configuration file for the Sphinx documentation builder.
#
# Juvix Manual documentation build configuration file
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
import sphinx_rtd_theme
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

# General information about the project.
project = 'Juvix'
copyright = '2021, Heliax'
author = 'Heliax Dev Team'

# The short X.Y version.
version = '0.0'

# The full version, including alpha/beta/rc tags
release = '0.0'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.todo',
#    'sphinx.ext.pngmath', # imgmath is not supported on readthedocs.
    'sphinx.ext.ifconfig',
    "sphinx_rtd_theme",
    "myst_parser",
    "sphinx_proof",
    "sphinxcontrib.tikz",
    "sphinxcontrib.bibtex"
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The master toctree document.
master_doc = 'index'

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
# # Read The Docs Themes specific settings
html_theme = 'sphinx_rtd_theme'
html_theme_options = {
    'display_version': True,
    'prev_next_buttons_location': 'bottom'
}




# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']
html_static_path = []
# Output file base name for HTML help builder.
htmlhelp_basename = 'JuvixManualdoc'

# -- Options for LaTeX output ---------------------------------------------

latex_title_page = r'''
\begin{titlepage}
    \vspace*{\fill}
    \begin{center}
        \includegraphics[width=0.25\textwidth]{../../Juvix_logo.png}\par
        \vspace{1cm}
        {\huge\sffamily\bfseries \makeatletter\@title\makeatother\par}
        \vspace{1cm}
        {\Large Version \version\par}
    \end{center}
    \vspace*{\fill}
\end{titlepage}
'''

latex_elements = {
# The paper size ('letterpaper' or 'a4paper').
'papersize': 'a4paper',

'fontpkg': '',
'inputenc': '',
'utf8extra': '',
'releasename': 'Version',

# The font size ('10pt', '11pt' or '12pt').
'pointsize': '10pt',

# Additional stuff for the LaTeX preamble.
'preamble': r'''
\usepackage{lmodern}
\usepackage[T1]{fontenc}
\usepackage[utf8x]{inputenc}
\usepackage{titlesec}
%
\usepackage{fancyhdr}
\fancypagestyle{plain}{%
  \renewcommand{\headrulewidth}{0pt}%
  \fancyhf{}%
  \fancyfoot[C]{\textsf{\thepage}}
}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[LE,RO]{\textsf{\bfseries{v\version}}}
\fancyhead[LO,RE]{\textsf{\bfseries\leftmark}}
%\fancyhead[LO]{\textsf{\bfseries{\leftmark}}}
%\fancyhead[RO]{\textsf{\bfseries{v\version}}}
%\fancyhead[RE]{\textsf{\bfseries{\leftmark}}}
%\fancyhead[LE]{\textsf{\bfseries{v\version}}}
\fancyfoot[C]{\textsf{\thepage}}
\renewcommand{\footrulewidth}{0pt}
\renewcommand{\headrulewidth}{0pt}
%
\usepackage[font={small,it}]{caption}
\titleformat{\section}
  {\normalfont\sffamily\Large\bfseries\color{black}}
  {\thesection}{1em}{}
\titleformat{\subsection}
  {\sffamily\large\bfseries\color{black}}
  {\thesubsection}{1em}{}
\titleformat{\subsubsection}
  {\sffamily\normalsize\bfseries\color{black}}
  {\thesubsubsection}{1em}{}
\titleformat{\paragraph}{\normalfont\normalsize\slshape}{\theparagraph}{1em}{}
\setlength{\parskip}{1em}
%
\hypersetup{colorlinks = false}
\definecolor{VerbatimBorderColor}{rgb}{1,1,1}
''',

'maketitle': latex_title_page,
'tableofcontents': "\\tableofcontents"
# Latex figure (float) alignment
#'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
#    ('tutorial/index',  'juvix-tutorial.tex',  u'The Juvix Tutorial',    u'The Juvix Community', 'howto'),
]


latex_show_pagerefs = True
latex_show_url = 'footnote'

# The name of an image file (relative to this directory) to place at the top of
# the title page.
latex_logo = '../../Juvix_logo.png'

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
#latex_use_parts = True

# If true, show page references after internal links.
#latex_show_pagerefs = False

# If true, show URL addresses after external links.
#latex_show_urls = False

# Documents to append as an appendix to all manuals.
#latex_appendices = []

# If false, no module index is generated.
#latex_domain_indices = True

bibtex_bibfiles = ['refs.bib']

# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'juvixmanual', u'Juvix Manual Documentation',
     [author], 1)
]

# If true, show URL addresses after external links.
#man_show_urls = False


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  (master_doc, 'JuvixManual', u'Juvix Manual Documentation',
   author, 'JuvixManual', 'One line description of project.',
   'Miscellaneous'),
]

# Documents to append as an appendix to all manuals.
#texinfo_appendices = []

# If false, no module index is generated.
#texinfo_domain_indices = True

# How to display URL addresses: 'footnote', 'no', or 'inline'.
#texinfo_show_urls = 'footnote'

# If true, do not generate a @detailmenu in the "Top" node's menu.
#texinfo_no_detailmenu = False


# -- Options for Epub output ----------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project
epub_author = author
epub_publisher = author
epub_copyright = copyright

# The basename for the epub file. It defaults to the project name.
#epub_basename = project

# The HTML theme for the epub output. Since the default themes are not optimized
# for small screen space, using the same theme for HTML and epub output is
# usually not wise. This defaults to 'epub', a theme designed to save visual
# space.
#epub_theme = 'epub'

# The language of the text. It defaults to the language option
# or 'en' if the language is not set.
#epub_language = ''

# The scheme of the identifier. Typical schemes are ISBN or URL.
#epub_scheme = ''

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#epub_identifier = ''

# A unique identification for the text.
#epub_uid = ''

# A tuple containing the cover image and cover page html template filenames.
#epub_cover = ()

# A sequence of (type, uri, title) tuples for the guide element of content.opf.
#epub_guide = ()

# HTML files that should be inserted before the pages created by sphinx.
# The format is a list of tuples containing the path and title.
#epub_pre_files = []

# HTML files shat should be inserted after the pages created by sphinx.
# The format is a list of tuples containing the path and title.
#epub_post_files = []

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']

# The depth of the table of contents in toc.ncx.
#epub_tocdepth = 3

# Allow duplicate toc entries.
#epub_tocdup = True

# Choose between 'default' and 'includehidden'.
#epub_tocscope = 'default'

# Fix unsupported image types using the Pillow.
#epub_fix_images = False

# Scale large images.
#epub_max_image_width = 0

# How to display URL addresses: 'footnote', 'no', or 'inline'.
#epub_show_urls = 'inline'

# If false, no index is generated.
#epub_use_index = True

myst_enable_extensions = [
    "amsmath",
    "colon_fence",
    "deflist",
    "dollarmath",
    "html_admonition",
    "html_image",
    # "linkify",
    "replacements",
    "smartquotes",
    "substitution",
    "tasklist",
]