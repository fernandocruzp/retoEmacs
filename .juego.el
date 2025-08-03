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
2. En el segundo párrafo de contenido.txt, corta la segunda oración (la que empieza con ""Antes lo relacionaba..."") y pégala al final de nivel2Completado.txt.
3. En nivel2Completado.txt, reemplaza todas las apariciones de la palabra ""mi"" por ""tú"".
4. En contenido.txt, reemplaza todas las apariciones de la palabra ""tú"" por ""mi"".

Nota: Los párrafos están separados por líneas en blanco. Las oraciones dentro de cada párrafo están separadas por puntos seguidos.

Cuando termines, guarda todos los archivos. Si todo está correcto, el nivel 3 se desbloqueará.
")))

    ;; Crear nivel2Completado vacío si no existe
    (unless (file-exists-p archivo-completado)
      (with-temp-file archivo-completado
        (insert "")))))


;; Funciones auxiliares para verificar-nivel-2

(defun leer-archivo-seguro (ruta)
  "Lee un archivo de forma segura, retorna nil si hay error."
  (condition-case nil
      (with-temp-buffer 
        (insert-file-contents ruta) 
        (buffer-string))
    (error nil)))

(defun parsear-contenido-original (texto)
  "Parsea el contenido original y retorna una estructura con párrafos y oraciones."
  (let* ((parrafos (split-string texto "\n\n+"))
         (parrafo-2 (nth 1 parrafos))
         (oraciones-p2 (split-string parrafo-2 "\\.")))
    (list :parrafos parrafos
          :parrafo-2 parrafo-2
          :oraciones-p2 oraciones-p2)))

(defun construir-texto-esperado-completado (estructura)
  "Construye las dos versiones esperadas del archivo completado."
  (let* ((parrafos (plist-get estructura :parrafos))
         (oraciones-p2 (plist-get estructura :oraciones-p2))
         (parrafo-1 (nth 0 parrafos))
         (parrafo-3 (nth 2 parrafos))
         (segunda-oracion (nth 1 oraciones-p2)))
    
    (list :sin-punto
          (replace-regexp-in-string
           "\\bmi\\b" "tú"
           (mapconcat 'identity
                      (list parrafo-1 parrafo-3 segunda-oracion) "\n\n"))
          :con-punto
          (replace-regexp-in-string
           "\\bmi\\b" "tú"
           (mapconcat 'identity
                      (list parrafo-1 parrafo-3 (concat segunda-oracion ".")) "\n\n")))))

(defun construir-texto-esperado-contenido (estructura)
  "Construye el texto esperado para contenido.txt (sin la segunda oración)."
  (let* ((parrafos (plist-get estructura :parrafos))
         (oraciones-p2 (plist-get estructura :oraciones-p2))
         (parrafo-1 (nth 0 parrafos))
         (parrafo-3 (nth 2 parrafos))
         (parrafo-4 (nth 3 parrafos))
         (parrafo-5 (nth 4 parrafos))
         ;; Reconstruir párrafo 2 sin la segunda oración
         (parrafo-2-nuevo
          (mapconcat 'identity
                     (list (nth 0 oraciones-p2) 
                           (nth 2 oraciones-p2) 
                           (nth 3 oraciones-p2) "") ".")))
    
    (replace-regexp-in-string
     "\\btú\\b" "mi"
     (mapconcat 'identity
                (list parrafo-1 parrafo-2-nuevo parrafo-3 parrafo-4 parrafo-5)
                "\n\n"))))

(defun validar-solucion-nivel-2 (texto-completado texto-contenido estructura)
  "Valida si la solución del nivel 2 es correcta."
  (let* ((esperado-completado (construir-texto-esperado-completado estructura))
         (esperado-contenido (construir-texto-esperado-contenido estructura))
         (completado-correcto 
          (or (string= texto-completado (plist-get esperado-completado :sin-punto))
              (string= texto-completado (plist-get esperado-completado :con-punto))))
         (contenido-correcto 
          (string= texto-contenido esperado-contenido)))
    (cond
     ((and completado-correcto contenido-correcto)
      (list :resultado :exito :mensaje "¡Nivel 2 completado!"))
     ((not completado-correcto)
      (list :resultado :error :mensaje "El archivo nivel2Completado no está correcto. Revisa que hayas copiado los párrafos 1 y 3, cortado la segunda oración del párrafo 2, y reemplazado 'mi' por 'tú'."))
     ((not contenido-correcto)
      (list :resultado :error :mensaje "El archivo contenido.txt no está correcto. Revisa que hayas cortado la segunda oración del párrafo 2 y reemplazado 'tú' por 'mi'.")))))

(defun restaurar-archivos-nivel-2 ()
  "Restaura los archivos a su estado original."
  (let ((contenido-original (expand-file-name "./contenido-original"))
        (contenido (expand-file-name "./contenido.txt"))
        (completado (expand-file-name "./nivel2Completado")))
    
    (condition-case err
        (progn
          (copy-file contenido-original contenido :overwrite)
          (when (file-exists-p completado)
            (delete-file completado))
          t)
      (error 
       (message "Error al restaurar archivos: %s" (error-message-string err))
       nil))))

(defun verificar-nivel-2 ()
  "Verifica si el archivo nivel2Completado contiene la bandera esperada y crea el nivel 3."
  (when (and buffer-file-name
             (string-match "nivel2Completado" buffer-file-name))
    
    (let* ((contenido-original-path (expand-file-name "./contenido-original"))
           (contenido-path (expand-file-name "./contenido.txt"))
           (completado-path (expand-file-name "./nivel2Completado"))
           
           ;; Leer archivos de forma segura
           (texto-original (leer-archivo-seguro contenido-original-path))
           (texto-contenido (leer-archivo-seguro contenido-path))
           (texto-completado (leer-archivo-seguro completado-path)))
      
      (cond
       ;; Verificar que todos los archivos se pudieron leer
       ((not texto-original)
        (message "Error: No se pudo leer el archivo contenido-original"))
       
       ((not texto-contenido)
        (message "Error: No se pudo leer el archivo contenido.txt"))
       
       ((not texto-completado)
        (message "Error: No se pudo leer el archivo nivel2Completado"))
       
       ;; Si todos los archivos se leyeron correctamente, validar
       (t
        (let* ((estructura (parsear-contenido-original texto-original))
               (validacion (validar-solucion-nivel-2 texto-completado texto-contenido estructura)))
          
          (if (eq (plist-get validacion :resultado) :exito)
              (progn
                (message (plist-get validacion :mensaje))
                (crear-nivel-3))
            (progn
              (message (plist-get validacion :mensaje))
              (restaurar-archivos-nivel-2)))))))))

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
