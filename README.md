sibPlot
===========

sibPlot is a project consisting in creating database formats and utilities for plant plot data in SiB Colombia (Sistema de Información sobre la Biodiversidad de Colombia).

# News

* December 29th, 2021: The package release was planned for December 28th. Unfortunately, some small difficulties(due to compatibility between PostgreSQL, SQLite and R) caused some delays in the programming work. Therefore I plan to release the first version of the package by mid-january...

# Objectives

The objectives here are to simply allows people who are no expert of SQL to create a database (from R, but in a SQL SGBD) for their vegetation plot data (permanent plots, temporal plots, potentially phytosociology relevés, with trait measurement, etc).
It is also a way to start making plot data format for the SiB Colombia.
So, the idea is to create a package that allows to manage:

1. reading files following one of the standard inputs for vegetation plots (modules might be added if your organization uses other formats)
1. storing the data in an efficient, structured SQL database (we will first focus on SQLite/Spatialite and postgreSQL/postgis)
1. exporting the database, or part of it in various formats which are used in the main collaboration platforms for plot data (forestPlot formats, DarwinCore, etc.)
1. integrating various features, tools and utilities to represent and describe data (maps, downloading catalog references, integrating traits, taxonomy, etc.)
1. documenting all the data, field and laboratory processes in preformatted html/pdf pages, thanks to the knitr/rmarkdown tools

# History

This repository consisted first on merging preliminary work made by Roy Gonzalez (@roy-gm) in the [roy-gm/rdsPermanentPlots](https://github.com/roy-gm/rdsPermanentPlots.git) (forked in [marbotte/rdsPermanentPlots](https://github.com/marbotte/rdsPermanentPlots)), and by myself (Marius Bottin: @marbotte) in the [marbotte/sib_plot_temp](https://github.com/marbotte/sib_plot_temp.git) repository.
I was not quite sure how to do that, and I am still not sure I used the right git solutions, but after 2 days of searching the big net, I decided to follow the explications from here: https://saintgimp.org/2013/01/22/merging-two-git-repositories-into-one-repository-without-losing-file-history/

# Programing philosophy

We hope that one day we will be able to show some nice, clean code, but right now, we are trying to go fast and to provide some good features... 
Therefore we'll accept some ugly fast coding for now.
The idea is to go into the development by writing scripts and rmarkdown document which describe the programming process.

If you find some of the code really difficult to understand, and know programation rules that might help clarify them, please do not hesitate to contact us (through issues, or private messages, or whatever way you may find to contact us).

# Data availability

In order to create the formats of the input/output and the storing, we use concrete, real data from a project (managed by the Instituto Humboldt).
Unfortunately, this data is not open access, so some of the codes you'll find in the repository might not make completely sense to you.
In the future, we'll try to find some example open access data which might help, but for now we do not have any way to resolve that issue

# File Organization

The repository consists in various directories:

* **devDocs** (development documents) is where I work the most right now: consists in rmarkdown documents in order to present and resolve the issues, and to develop functions, together with examples of applications
* **functions** is where the finalized functions should go when they are ready, it is a good practice to put there the functions that might be used in more than one development document as well
* **inputSpec** is the way I store the input specifications for now, but on the long term, this should be stored in the databases themselves...
* **pkg** is where we will store the R packages
* **scripts** are the piece of R code which are neither devDocs (rmarkdown documents) nor R functions. There are pieces of codes which are "scripts" for various applications (I notably put there all the code that Roy developed, in order to be able to get some pieces of his code into functions

# How to contribute

Right now, I (@marbotte) will be the main contributor of the repository, and until december, 2021, I'll be almost 100% of my time working on it, so the easiest way to collaborate might be through branches. Do not hesitate to ask me to put you as a collaborator here.

You may as well fork and PR as you wish!
