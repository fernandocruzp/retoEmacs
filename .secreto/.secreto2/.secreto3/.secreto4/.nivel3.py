import sys

def leeClave():
    clave = ""
    for i in range(0, 21):
        archivo = open("bi{}.txt".format(i), "r")
        clave += archivo.read()
        archivo.close()
    return clave

texto_nivel4 = """\
Nivel 4. Macros

En la raíz encontrarás un archivo llamado nivel4.el, este archivo contiene funciones emacs lisp que te darán una configuración inicial de emacs para tus primeros años de la carrera, pero no funciona, tiene unos pequeños errores sintácticos: todos los [] no deberían estar ahí, bórralos inteligentemente, usa macros.

Una vez que tengas el archivo listo ejecuta:

  M-x load-file RET nivel4.el RET
  M-x nivel4-iniciar RET

Si todo sale bien tendrás un archivo de configuración en ~/.emacs.d/init.el y acceso al último nivel.
"""

codigo_nivel4 = """\
;; Archivo con errores sintácticos deliberados para practicar macros
(defun nivel4-iniciar () [][]
  (interactive)
  (nivel4-instala-paquetes)[]
  (nivel4-crea-configuracion)
  (nivel4-crea-nivel5))

(defun nivel4-instala-paquetes ()
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)[]
  (dolist (pkg '(use-package haskell-mode elpy lsp-java))
    ([unless] (package-installed-p pkg)
      (package-install pkg))))

(defun nivel4-crea-configuracion ()
  (let ((ruta (expand-file-name "init.el" user-emacs-directory)))
    (with-temp-file ruta
      (insert ";; Configuración básica para estudiantes de computación
")
      (insert "(require 'package)
")[][][][]
      (insert "(add-to-list 'package-archives '(""melpa"" . ""https://melpa.org/packages/"") t)
")[]
      (insert "(package-initialize)
")
      (insert "(load-theme 'dracula t)
")
      (insert "(use-package haskell-mode :ensure t)
")
      (insert "(use-package elpy :ensure t)
")
      (insert "(elpy-enable)
")
      (insert "(use-package lsp-java :ensure t)
")
      (insert "(use-package cc-mode :ensure t)
")
      (insert "(setq-default indent-tabs-mode nil)
")
      (insert "(setq-default tab-width 4)
"))))[][]

[][][]
(defun nivel4-crea-nivel5 ()
  (let[ ((ruta "./niveles/nivel5.txt"))]
    (with-temp-file ruta
      (insert "Correo: cruzfernando@ciencias.unam.mx
")
      (insert "Envía[ un correo desde] Emacs con el asunto 'LOgrado' y tu[] nombre completo.
")))
)

"""

def creaNivel4():
    with open("./niveles/nivel4.txt", "w") as archivo_txt:
        archivo_txt.write(texto_nivel4)

    with open("nivel4.el", "w") as archivo_el:
        archivo_el.write(codigo_nivel4)

if len(sys.argv) > 1:
    if sys.argv[1] == leeClave():
        print("Siguiente nivel")
        creaNivel4()
    else:
        print("Esa no es la clave")
else:
    print("Debes agregar la clave secreta")
