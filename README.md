# Text2Surv

## Guia instalación python con entorno anaconda

### Descargar e instalar anaconda

	- En Windows: https://docs.anaconda.com/free/anaconda/install/windows/

	- En macOS: https://docs.anaconda.com/free/anaconda/install/mac-os/

### Crear entorno anaconda con Python 

	- Abrir anaconda prompt (Windows)

	- Abrir cmd (macOs)

	- Navegar hasta el directorio principal de la app (Text2Surv)

	- conda create --prefix=.venv python=3.9


### Preparar entorno anaconda

	- conda activate "Path"\.venv

 	- conda install numpy

 	- conda install pandas

	- Navegar al directorio donde se encuentre la carpeta con la libreria de galen a instalar (./fragments/ , ./tools/ etc) y ejecutar "pip install -e ." Se debe seguir el siguiente orden:

		-- fragments

		-- tools

		-- brat

		-- tokens

		-- corpus

		-- tnm

	- conda install -c anaconda regex

	- conda install gensim

	- conda install scikit-learn


	
