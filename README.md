------------------------------------------------------------------------

# Proyecto-Deep-Learning---Enfermedades-Visuales

Proyecto grupal para la asignatura Electiva de Deep Learning - Ingeniería Estadística

Clasificación de Patologías Oculares mediante Deep Learning Este repositorio contiene la implementación y el análisis de modelos de Deep Learning para la clasificación de 8 enfermedades oculares utilizando la base de datos ODIR.

El enfoque principal de este proyecto es el de aprender y replicar. Es decir: comprender los fundamentos matemáticos y computacionales detrás de las Redes Neuronales Convolucionales (CNN) y abordar empíricamente los desafíos del sobreajuste (overfitting) causados por la escasez de imágenes médicas.

🎯 Objetivos e Implementación base: Construir y entrenar una CNN desde cero para comprender la extracción de características y observar los problemas de generalización ante datos limitados. Implementación avanzada: Aplicar Transfer Learning utilizando arquitecturas preentrenadas (como VGG16 o ResNet) para mitigar la falta de datos y mejorar la precisión clínica.

Evaluación crítica: Contrastar el rendimiento de ambos modelos analizando sus curvas de aprendizaje y matrices de confusión para justificar el uso de herramientas avanzadas en la industria médica.

🗂️ Base de Datos Se utiliza el conjunto de datos ODIR (Ocular Disease Intelligent Recognition), que contiene imágenes de fondo de ojo etiquetadas en 8 categorías : Normal (N) Diabetes (D) Glaucoma (G) Cataratas (C) Degeneración Macular Relacionada con la Edad (A) Miopía (M) Hipertensión (H) Otras anomalías (O) 🛠️ Tecnologías y Metodología Herramientas: Python, TensorFlow / Keras, NumPy, Pandas, Matplotlib. Modelo 1 (Desde cero): Arquitectura CNN secuencial básica para análisis pedagógico. Modelo 2 (Transfer Learning): Adaptación de un modelo profundo (VGG16 / ResNet) congelando capas iniciales y reentrenando el clasificador final.

📂 Estructura del Repositorio data/: Scripts para la descarga, normalización y redimensionamiento de las imágenes de ODIR. notebooks/: Cuadernos Jupyter con la construcción paso a paso, entrenamiento y evaluación comparativa. src/: Código fuente modular con la definición de las arquitecturas de red. models/: Directorio para almacenar los pesos de los modelos entrenados.
