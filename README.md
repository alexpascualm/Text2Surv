# Text2Surv
---

## 📋 Descripción general

Este proyecto nace del Trabajo de Fin de Grado “Clasificación de Neoplasias mediante procesamiento de textos para el estudio de supervivencia poblacional” (Alejandro Pascual Mellado, Universidad de Málaga, junio 2024).  
Su objetivo principal es:
1. Proporcionar una aplicación web para carga masiva de datos (hojas de cálculo y PDFs), extracción de información automática, gestión, visualización y análisis estadístico (curvas de supervivencia de Kaplan–Meier, estadísticas descriptivas, filtros, gráficas) en un entorno clínico local.
2. Extraer la localización de neoplasias (CIE‑10‑ES) de historias clínicas electrónicas (HCE) en español, usando modelos de NLP.
3. Facilitar la labor administrativa de los servicios de oncológia favoreciendo la transformación digital hospitalaria.
---

## ⚙️ Instalación de la aplicación

La aplicación corre completamente en local, sin conexiones externas, garantizando la privacidad de los datos clínicos.

### 1. Prerrequisitos
   - R (≥ 4.0)  
   - RStudio Desktop  
   - Anaconda (Python 3.9)

### 2. Crear y preparar entorno Python

   Desde Anaconda Prompt (Windows) o Terminal (macOS/Linux):

	   conda create --prefix .venv python=3.9  
	   conda activate ./.venv  
	   conda install numpy pandas regex scikit-learn gensim  

   Instalar librerías del proyecto (orden crítico):
   
	   cd modules/fragments && pip install -e .  
	   cd ../tools     && pip install -e .  
	   cd ../brat      && pip install -e .  
	   cd ../tokens    && pip install -e .  
	   cd ../corpus    && pip install -e .  
	   cd ../tnm       && pip install -e .

### 3. Instalar dependencias R

   Abre RStudio, fija tu directorio de trabajo en la raíz del proyecto e instala las dependencias cargadas en app.R:

### 4. Ejecutar la aplicación

   En RStudio, abre app.R y pulsa Run App.  
   
---

## 🚀 Uso principal

1. Login: sistema de usuarios + SQLite.  
2. Carga de datos:
   - Hoja de cálculo: XLSX/CSV con columnas clave.  
   - ZIP de PDFs: extracción automática con expresiones regulares.  
3. Gestión de registros: visualización, edición y filtrado dinámico.  
4. Análisis descriptivo: tablas y gráficas según tipo de campo (numérico, fecha, categórico).  
5. Parámetros de gráfica: personaliza títulos y ejes.  
6. Supervivencia: curvas de Kaplan–Meier (básico, estratificado, con cálculo de tiempo).  
7. Inferencia AI: localización de neoplasias mediante modelos de procesamiento de lenguaje natural integrados para clasificar historiales clínicos electrónicos.

---

## 👩‍💻 Herramientas y tecnologías

- RShiny para la interfaz web interactiva.  
- reticulate para interoperabilidad R ↔️ Python.   
- SQLite para el backend de usuarios y metadatos.  
- Expresiones regulares para extraer datos de PDFs.
- PLN para procesamiento y clasificación de textos





	
