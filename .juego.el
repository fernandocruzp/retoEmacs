(defun verificar-nivel-1 ()
  "Verifica si el archivo nivel1Completado contiene la bandera esperada y crea el nivel 2."
  (when (and buffer-file-name
             (string-match "nivel1Completado" buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "Bandera encontrada" nil t)
        (message "¡Nivel 1 completado! Generando nivel 2...")
        (crear-nivel-2)))))

(defun crear-nivel-2 ()
  "Crea el archivo para el nivel 2 y el archivo nivel2Completado.txt en el directorio raíz."
  (let ((archivo-nivel (expand-file-name "./niveles/nivel2.txt"))
        (archivo-completado (expand-file-name "./nivel2Completado")))
    
    ;; Crear nivel2.txt si no existe
    (unless (file-exists-p archivo-nivel)
      (with-temp-file archivo-nivel
        (insert "Nivel 2 – Edición con múltiples ventanas

Abre tres ventanas en Emacs. Se recomienda:

- Dos verticales a los lados
- Una horizontal abajo

Carga en ellas los siguientes archivos:
- nivel2.txt (este archivo)
- contenido.txt
- nivel2Completado.txt

Tareas:

1. Copia los párrafos 1 y 3 de contenido.txt en nivel2Completado.txt.
2. Corta la línea 2 del segundo párrafo de contenido.txt y pégala al final en nivel2Completado.txt.
3. En nivel2Completado.txt, reemplaza todas las apariciones de mi por tú.
4. En contenido.txt, reemplaza todas las apariciones de tu por mi.

Cuando termines, guarda todos los archivos. Si todo está correcto, el nivel 3 se desbloqueará.
")))

    ;; Crear nivel2Completado vacío si no existe
    (unless (file-exists-p archivo-completado)
      (with-temp-file archivo-completado
        (insert "")))))


(defun verificar-nivel-2 ()
  "Verifica si el archivo nivel2Completado contiene la bandera esperada y crea el nivel 3."
  (when (and buffer-file-name
             (string-match "nivel2Completado" buffer-file-name))
    (let* ((contenido-original (expand-file-name "./contenido-original"))
           (contenido (expand-file-name "./contenido.txt"))
           (completado (expand-file-name "./nivel2Completado"))
           ;; Leer archivos
           (texto-contenido (with-temp-buffer (insert-file-contents contenido) (buffer-string)))
           (texto-original (with-temp-buffer (insert-file-contents contenido-original) (buffer-string)))
           (texto-completado (with-temp-buffer (insert-file-contents completado) (buffer-string)))
           ;; Construir texto esperado para nivel2Completado
           (parrafos (split-string texto-original "\n\n+"))
           (parrafo-1 (nth 0 parrafos))
           (parrafo-2 (nth 1 parrafos))
           (parrafo-3 (nth 2 parrafos))
           (parrafo-4 (nth 3 parrafos))
           (parrafo-5 (nth 4 parrafos))
           ;; Obtener línea 5 del párrafo 2
           (linea-1 (nth 0 (split-string parrafo-2 "\\.")))
           (linea-2 (nth 1 (split-string parrafo-2 "\\.")))
           (linea-3 (nth 2 (split-string parrafo-2 "\\.")))
           (linea-4 (nth 3 (split-string parrafo-2 "\\.")))
           (parrafo-2-nuevo
            (mapconcat 'identity
                       (list linea-1 linea-3 linea-4 "") "."))
           (esperado-completado
            (replace-regexp-in-string
             "\\bmi\\b" "tú"
             (mapconcat 'identity
                        (list parrafo-1 parrafo-3 linea-2) "\n\n")))
           (esperado-contenido
            (replace-regexp-in-string
             "\\btú\\b" "mi"
             (mapconcat 'identity
                        (list parrafo-1 parrafo-2-nuevo parrafo-3 parrafo-4 parrafo-5)
                        "\n\n"))))
      (if (and (string= texto-completado esperado-completado)
               (string= texto-contenido esperado-contenido))
          (message "¡Nivel 2 completado!")
	(crear-nivel-3)
        (progn
          (copy-file contenido-original contenido :overwrite)
          (delete-file completado)
          (message "Algo salió mal. Se restauraron los archivos."))))))

(defun crear-nivel-3 ()
  "Crea el archivo para el nivel 3 y los buffers necesarios."
  (let ((archivo-nivel (expand-file-name "./niveles/nivel3.txt"))
        (letras '("A" "L" "A" "N" "T" "U" "R" "I" "N" "G"
                  "L" "O" "N" "Z" "O" "C" "H" "U" "R" "C" "H")))
    
    ;; Crear nivel3.txt si no existe
    (unless (file-exists-p archivo-nivel)
      (with-temp-file archivo-nivel
        (insert "Nivel 3 – La terminal y buffers

Abre la terminal dentro de Emacs y busca un archivo .py secreto, muévelo a la raíz de esta carpeta

Tareas:

1. Se abrieron varios buffers llamados bi<n>.txt 
2. En cada uno encontrarás una letra
3. Concatena cada una de estas letras para generar la clave secreta
4. Ejecuta el archivo .py oculto con: python <archivo_secreto>.py <clave>, también puedes usar el comando `M-!` en Emacs

Cuando ejecutes el programa correctamente podrás pasar al siguiente nivel.
")))

    ;; Crear archivos bi<n>.txt con letras
    (dotimes (i (length letras))
      (let* ((nombre (format "./bi%d.txt" i))
             (ruta (expand-file-name nombre)))
        (unless (file-exists-p ruta)
          (with-temp-file ruta
            (insert (nth i letras))))
        (find-file ruta)))
    (find-file archivo-nivel)
    )
  )  ;; abre cada buffer en otro buffer



(add-hook 'before-save-hook #'verificar-nivel-1)
(add-hook 'before-save-hook #'verificar-nivel-2)
