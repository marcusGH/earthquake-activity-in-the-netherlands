CID = 01725740

# Utility function for submitting my files to TurnitIn
package:
	mkdir temp-package-dir
	# move all the files tracked by git to this folder, maintaining the folder structure
	git ls-files | rsync -a --files-from=- --prune-empty-dirs . temp-package-dir
	cd temp-package-dir ; zip -r ../$(CID)-project-2.zip *
	# clean up
	rm -r temp-package-dir

# Creates the plots and builds the monthly summary
monthly_summary:
	# create the plots
	Rscript src/monthly-summary/00_minimum-magnitude-of-completeness.R
	Rscript src/monthly-summary/01_earthquake-visualisations.R
	# compile the monthly summary and move a copy of it to root
	latexmk -pdf -pdflatex -output-directory="reports/monthly-summary/build/" -verbose -synctex=1 -shell-escape reports/monthly-summary/monthly-summary.tex
	cp reports/monthly-summary/build/monthly-summary.pdf $(CID)-monthly-summary.pdf

# Creates the plots and builds the MCMC storage guide
mcmc_storage_guide:
	# create the plots
	Rscript src/mcmc-storage-guide/00_mcmc-storage-guide-simulation-study.R
	# compile the storage guide and move a copy of it to root
	latexmk -pdf -pdflatex -output-directory="reports/mcmc-storage-guide/build/" -verbose -synctex=1 -shell-escape reports/mcmc-storage-guide/mcmc-storage-guide.tex
	cp reports/mcmc-storage-guide/build/mcmc-storage-guide.pdf $(CID)-mcmc-storage-guide.pdf
